(defpackage #:clcomp.ssa.opt
  (:use #:cl))

(in-package :clcomp.ssa.opt)

;; this is just simple LOAD VIRTUAl, VIRTUAL elimination so gdb tracing is easier to follow
(defun copy-prop-1 (lambda-ssa)
  (let ((loads (make-hash-table))
	(ir-indexes (make-hash-table)))
    (dolist (b (clcomp.ssa::lambda-ssa-blocks lambda-ssa))
      (dolist (ir (clcomp.ssa::ssa-block-ssa b))
	(typecase ir
	  (clcomp.ssa::ssa-load (let ((to (clcomp.ssa::ssa-load-to ir))
				      (from (clcomp.ssa::ssa-load-from ir))
				      (ir-index (clcomp.ssa::ssa-load-index ir)))
				  (when (and (clcomp.ssa::virtual-place-p to)
					     (clcomp.ssa::virtual-place-p from))
				    (setf (gethash to loads) from)
				    (setf (gethash to ir-indexes) ir-index))))
	  (otherwise t))))
    (values loads ir-indexes)))

(defun get-final-replace (loads v1 &optional current)
  (let ((v2 (gethash v1 loads)))
    (if v2
	(get-final-replace loads v2 v2)
	current)))

(defun copy-prop-replace-phi-operands (lambda-ssa loads)
  (declare (optimize debug))
  (let ((replacable (clcomp::hash-keys loads)))
    (dolist (phi (clcomp::hash-values (clcomp.ssa::lambda-ssa-all-phis lambda-ssa)))
      (when (and (clcomp.ssa::phi-p phi)
		 (not (clcomp.ssa::get-phi-place-reduced-value (clcomp.ssa::phi-place phi))))
	(let ((old-places (intersection replacable (clcomp.ssa::phi-operands phi))))
	  (when old-places
	    (dolist (old old-places)
	      (let ((index (position old (clcomp.ssa::phi-operands phi)))
		    (replace (get-final-replace loads old)))
		(assert replace)
		(setf (nth index (clcomp.ssa::phi-operands phi)) replace)))))))
    ;; patch live-phi-operands
    (dolist (b (clcomp.ssa::lambda-ssa-blocks lambda-ssa))
      (let* ((live-phi-operands (clcomp.ssa::ssa-block-live-phi-operands b))
	     (places (mapcar #'cdr live-phi-operands))
	     (olds (intersection replacable places)))
	(when olds
	  (dolist (old olds)
	    ;; PLACES jave the same order as LIVE-PHI-OPERANDS
	    (let ((index (position old places))
		  (replace (get-final-replace loads old)))
	      (assert replace)
	      (setf (cdr (nth index live-phi-operands)) replace))))))))

(defun copy-prop-1-patch-lambda (lambda-ssa loads ir-indexes)
  (let ((replacable (clcomp::hash-keys loads))
	(remove-indexes (clcomp::hash-values ir-indexes)))
    (dolist (b (clcomp.ssa::lambda-ssa-blocks lambda-ssa))
      (let ((filtered-irs nil))
	(dolist (ir (clcomp.ssa::ssa-block-ssa b))
	  (unless (find (clcomp.ssa::ssa-form-index ir) remove-indexes)
	    (typecase ir
	      (clcomp.ssa::ssa-form-rw
	       (let ((reads-write (clcomp.ssa::ssa-form-rw-reads-writes ir)))
		 (let ((old-places (intersection replacable (first reads-write))))
		   (when old-places
		     (dolist (old old-places)
		       (let ((replace (get-final-replace loads old)))
			 (assert replace)
			 (clcomp.ssa::ssa-form-rw-replace-reads ir old replace))))))))
	    (push ir filtered-irs)))
	(setf (clcomp.ssa::ssa-block-ssa b) (reverse filtered-irs))))))

;; TODO, we should actually re-run process of PHI simplifying
(defun copy-prop-fix-redundant-phis (lambda-ssa loads)
  (let* ((rphis (clcomp.ssa::lambda-ssa-redundant-phis lambda-ssa))
	 (keys (clcomp::hash-keys rphis)))
    (dolist (k keys)
      (let ((replace-value (gethash k rphis)))
	(when (gethash replace-value loads)
	  (setf (gethash k rphis) (get-final-replace loads replace-value)))))))

(defun do-copy-prop-1 (lambda-ssa)
  (declare (optimize debug))
  (multiple-value-bind (loads ir-indexes) (copy-prop-1 lambda-ssa)
    (when loads
      (copy-prop-1-patch-lambda lambda-ssa loads ir-indexes)
      (copy-prop-replace-phi-operands lambda-ssa loads)
      (copy-prop-fix-redundant-phis lambda-ssa loads))))


;; Constant Propagation with Conditional
;; Branches
;; MARK N. WEGMAN and F. KENNETH ZADECK
;; 3.4 Algorithm

;;; Our lattice symbols
;;; TOP -> not yet evaluated
;;; BOTTOM - > not a constant
;;; CONSTANT

;;; ANY merge TOP = ANY
;;; ANY merge BOTTOM = BOTTOM
;;; CONSTANT merge CONSTANT = CONSTANT  IF L = J
;;; CONSTANT merge CONSTANT = BOTTOM  IF L /= J

;;; NOTE, currentlu we don't have constant-folding info for functions or VOP's
;;; so we are not doing constant propagation across function calls and VOP calls
;;; but implementation will support that though

(defstruct ssa-estate def uses index-block)
(defun make-ssa-edges-state ()
  (make-ssa-estate :def (make-hash-table)
		   :uses (make-hash-table)
		   :index-block (make-hash-table)))

(defstruct ssa-funcall args returns fun)

(defun ssa-estate-clear-fun-args (state)
  (setf (ssa-estate-fun-args state) nil))

(defun ssa-state-add-use (state v use)
  (push (cons use (gethash v (ssa-estate-uses state)))
	(gethash v (ssa-estate-uses state))))

(defun ssa-state-add-def (state v def-form)
  (push (cons def-form (gethash v (ssa-estate-def state)))
	(gethash v (ssa-estate-def state))))

(defun ssa-estate-connect-ir-block (state index block-index)
  (setf (gethash index (ssa-estate-index-block state)) block-index))


;;; NOTE, I'm not happy with this, 
;;; here we are trying to reconstruct function call arguments places and return values
;;; in our current SSA IR we don't have this information in one instruction
;;; however we should do this on IR level, create instruction that will no affect register allocation and live intervals
;;; NOTE current implementation will maybe clash with future copy-propagation-2
(defun collect-function-call-data (instructions)
  (declare (optimize debug))
  (let ((fun-call-arguments nil)
	(fun-call-ir nil)
	(fun-call-returns nil))
    (tagbody loop
       (let ((ir (first instructions))
	     (rest (rest instructions)))
	 (typecase ir
	   (cssa::ssa-load
	    (let ((to (cssa::ssa-load-to ir))
		  (from (cssa::ssa-load-from ir)))
	      (cond ((cssa::argument-place-p to)
		     (push from fun-call-arguments))
		    ((cssa::return-value-place-p from)
		     (push to fun-call-returns))
		    ((or (cssa::function-value-place-p to)
			 (cssa::argument-count-place-p to)))
		    (t
		     (return-from collect-function-call-data
		       (list rest
			     (make-ssa-funcall :args fun-call-arguments
					       :fun fun-call-ir
					       :returns fun-call-returns)))))))
	   (cssa::ssa-mvb-bind
	    (let ((returns (cssa::ssa-mvb-bind-places ir)))
	      (return-from collect-function-call-data
		(list rest
		      (make-ssa-funcall :args fun-call-arguments
					:fun fun-call-ir
					:returns returns)))))
	   (cssa::ssa-fun-call
	    (assert (null fun-call-ir))
	    (setf fun-call-ir ir))
	   (otherwise  (return-from collect-function-call-data
			 (list instructions
			       (make-ssa-funcall :args fun-call-arguments
						 :fun fun-call-ir
						 :returns fun-call-returns)))))
	 (setf instructions rest)
	 (go loop)))))

;;; FIXME, we need to handlee SSA-RETURN as we do wwith SSA-FUN-CALL

(defun maybe-fun-call-istructions (ir)
  (typecase ir
    (cssa::ssa-load
     (let ((to (cssa::ssa-load-to ir))
	   (from (cssa::ssa-load-from ir)))
       (or (cssa::argument-place-p to)
	   (cssa::return-value-place-p from)
	   (cssa::function-value-place-p to)
	   (cssa::argument-count-place-p to)))) 
    (cssa::ssa-fun-call
     t)))

;;; there is complication here because how our SSA IR handles function call arguments and return values
(defun process-ssa-edges-from-block (state block)
  (declare (optimize debug))
  ;; process PHI's first
  (dolist (phi (cssa::ssa-block-phis block))
    (unless (cssa::get-phi-place-reduced-value (cssa::phi-place phi))
      (ssa-state-add-def state (cssa::phi-place phi) phi)
      (dolist (operand (cssa::phi-operands phi))
	(ssa-state-add-use state operand phi))))
  (let ((instructions (cssa::ssa-block-ssa block))
	(block-index (cssa::ssa-block-index block)))
    (tagbody loop
       (let ((ir (first instructions))
	     (rest (rest instructions)))
	 (when (null ir)
	   (go end))
	 (ssa-estate-connect-ir-block state (cssa::ssa-form-index ir) block-index)
	 (if (maybe-fun-call-istructions ir)
	     (let* ((fun-call (collect-function-call-data instructions))
		    (next-instructions (first fun-call))
		    (fun-call (second fun-call)))
	       (setf instructions next-instructions)
	       (dolist (r (ssa-funcall-returns fun-call))
		 (ssa-state-add-def state r fun-call))
	       (dolist (a (ssa-funcall-args fun-call))
		 (ssa-state-add-use state a fun-call))
	       (go loop))
	     (typecase ir
	       (cssa::ssa-form-rw
		(let* ((rw (cssa::ssa-form-rw-reads-writes ir))
		       (reads (first rw))
		       (writes (second rw)))
		  (dolist (write writes)
		    (typecase write
		      (cssa::named-place
		       (ssa-state-add-def state write ir))))
		  (dolist (read reads)
		    (typecase read
		      (cssa::named-place
		       (ssa-state-add-use state read ir))))))))
	 (setf instructions rest)
	 (go loop))
     end)))

;; FIXME, we should initialize with BOTTOM (for example for VALUE from RCV-ARGUMENT-PLACE) and other
(defun init-lattice-map (lambda-ssa)
  (declare (optimize debug))
  (let ((lattice-map (make-hash-table)))
    (dolist (b (clcomp.ssa::lambda-ssa-blocks lambda-ssa))
      (dolist (ir (clcomp.ssa::ssa-block-ssa b))
	(typecase ir
	  (cssa::ssa-form-rw 
	   (let* ((reads-writes (clcomp.ssa::ssa-form-rw-reads-writes ir))
		  (writes (second reads-writes )))
	     (dolist (write writes)
	       (when (or (clcomp.ssa::virtual-place-p write)
			 (clcomp.ssa::phi-place-p  write))
		 (assert (null (gethash write lattice-map)))
		 (setf (gethash write lattice-map) 'TOP))))))))
    lattice-map))

(defun init-ssa-edges (lambda-ssa)
  (declare (optimize debug))
  (let ((state (make-ssa-edges-state)))
    (dolist (b (clcomp.ssa::lambda-ssa-blocks lambda-ssa))
      (process-ssa-edges-from-block state b))
    state))

(defun visit-phi (b)
  (declare (ignore b)))

(defun visit-expressions (b lattice-map ssa-edges exec-flags)
  (dolist (ir (cssa::ssa-block-ssa b))
    (etypecase ir
      ((or cssa::lambda-entry cssa::arg-check
	   cssa::deallocate-function-frame cssa::allocate-stack
	   cssa::ssa-function-epilogue cssa::maybe-mv-adjust-stack cssa::ssa-label))
      (cssa::ssa-form-rw
       (etypecase ir
	 (cssa::ssa-load (let ((to (cssa::ssa-load-to ir))
			       (from (cssa::ssa-load-from ir)))
			   ())))))))

(defun sparse-conditional-constant-propagation (lambda-ssa)
  (let* ((flow-work-list nil)
	 (ssa-work-list nil)
	 (exec-flags (make-hash-table))
	 (ssa-edges (init-ssa-edges lambda-ssa))
	 (lattice-map (init-lattice-map lambda-ssa))
	 (block1 (first (clcomp.ssa::lambda-ssa-blocks lambda-ssa))))

    (multiple-value-bind (fwl swl) (visit-expressions block1 lattice-map ssa-edges exec-flags)
      )))

;;; NOTE after some hours working on SCCP
;;; My current SSA IR format is not friendly for SCCP, it is more low level than SCCP would like
;;; not just SCCP, I assume that other optimization passed would like more high level concise format
;;; TODO, create more high level SSA format for this phase



;; (defun add-copy (from to loads)
;;   (declare (optimize debug))
;;   (let ((chains (second loads)))
;;     (dolist (chain chains)
;;       (when (eq from (first (second chain)))
;; 	(push to (second chain))
;; 	(return-from add-copy)))
;;     (setf (second loads)
;; 	  (append (list (list nil (list to from)))
;; 		  (second loads)))))

;; (defun add-write (write loads)
;;   (print (list 'write write)))

;; (defun kill-chains (loads)
;;   (declare (optimize debug))
;;   (let ((chains (second loads)))
;;     (dolist (chain chains)
;;       (unless (eq 'kill (first (second chain)))
;; 	(push 'kill (second chain))))))

;; (defun copy-propagation (lambda-ssa)
;;   (let ((visited (make-hash-table))
;; 	(loads (list 'loads nil)))
;;     (dolist (b (lambda-ssa-blocks lambda-ssa))
;;       (unless (gethash (ssa-block-index b) visited)
;; 	(block-copy-propagation b visited loads lambda-ssa)))
;;     (break)
;;     loads))

;; (defun block-copy-propagation (b visited loads lambda-ssa)
;;   (declare (optimize debug))
;;   (setf (gethash (ssa-block-index b) visited) t)
;;   (dolist (ir (ssa-block-ssa b))
;;     (typecase ir
;;       (ssa-load (add-copy (ssa-load-from ir)
;; 			  (ssa-load-to ir)
;; 			  loads))
;;       (ssa-kill-form (kill-chains loads))
;;       (ssa-form-rw
;;        (let* ((rw (ssa-form-rw-reads-writes ir))
;; 	      (reads (first rw))
;; 	      (writes (second rw)))
;; 	 (declare (ignore reads))
;; 	 (dolist (w writes)
;; 	   (add-write w loads))))))
;;   (let ((successors-blocks (ssa-block-successors b lambda-ssa)))
;;     (if (< (length successors-blocks) 2)
;; 	(when (= 1 (length successors-blocks))
;; 	  (block-copy-propagation (first successors-blocks) visited loads lambda-ssa))
;; 	(progn
;; 	  (kill-chains loads)
;; 	  (dolist (bl successors-blocks)
;; 	    (unless (gethash (ssa-block-index b) visited)
;; 	      (block-copy-propagation bl visited loads lambda-ssa)))))))

