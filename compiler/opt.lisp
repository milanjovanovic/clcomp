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

;; FIXME, revisit this whole thing in the light of reduced PHI's
(defun do-copy-prop-1 (lambda-ssa)
  (declare (optimize debug))
  (multiple-value-bind (loads ir-indexes) (copy-prop-1 lambda-ssa)
    (when loads
      (copy-prop-1-patch-lambda lambda-ssa loads ir-indexes)
      (copy-prop-replace-phi-operands lambda-ssa loads)
      (copy-prop-fix-redundant-phis lambda-ssa loads))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant Propagation with Conditional
;; Branches
;; MARK N. WEGMAN and F. KENNETH ZADECK
;; 3.4 Algorithm

;;; Our lattice symbols
;;; TOP -> not yet evaluated
;;; BOTTOM - > not a constant
;;; CONSTANT = literal value (CSSA::IMMEDIATE-CONSTANT)
;;; FIXME, should FIXUP be a constant ? Any other type ?

;;; NOTE, currently we don't have constant-folding info for functions or VOP's
;;; so we are not doing constant propagation across function calls and VOP calls
;;; but implementation will support that though

(defun get-place (place)
  (etypecase place
    (cssa::phi (cssa::get-maybe-reduced-place (cssa::phi-place place)))
    (cssa::phi-place (cssa::get-maybe-reduced-place place))
    (cssa::named-place place)))

(defstruct ssa-graph def uses index-block flow-edges blocks reachable-blocks successors)

(defun make-default-graph (lambda-ssa)
  (let ((blocks (make-hash-table)))
    (dolist (b (cssa::lambda-ssa-blocks lambda-ssa))
      (setf (gethash (cssa::ssa-block-index b) blocks) b))
    (make-ssa-graph :def (make-hash-table)
		    :uses (make-hash-table)
		    :index-block (make-hash-table)
		    :flow-edges (make-hash-table :test 'equal)
		    :blocks blocks
		    :reachable-blocks (make-hash-table)
		    :successors (make-hash-table))))

(defun ssa-graph-get-use (graph v)
  (gethash v (ssa-graph-uses graph)))

(defun ssa-graph-add-use (graph v use)
  (push use
	(gethash v (ssa-graph-uses graph))))

(defun ssa-graph-add-def (graph v def-form)
  (push def-form
	(gethash v (ssa-graph-def graph))))

(defun ssa-graph-connect-ir-block (graph index block-index)
  (setf (gethash index (ssa-graph-index-block graph)) block-index))

(defun ssa-graph-get-ir-block-index (graph ir)
  (gethash (cssa::ssa-form-index ir) (ssa-graph-index-block graph)))

(defun ssa-graph-get-flow-edge-executable (graph edge)
  (gethash edge (ssa-graph-flow-edges graph)))

(defun ssa-graph-update-flow-edge-executable (graph edge)
  (let ((current-edge-value (gethash edge (ssa-graph-flow-edges graph))))
    (if current-edge-value
	(setf (gethash edge (ssa-graph-flow-edges graph)) (1+ current-edge-value))
	(setf (gethash edge (ssa-graph-flow-edges graph)) 1))))

(defun ssa-graph-get-block (graph block-index)
  (gethash block-index (ssa-graph-blocks graph)))

(defun ssa-graph-is-block-reachable (graph block-index)
  (gethash block-index (ssa-graph-reachable-blocks graph)))

(defun ssa-graph-set-block-reachable (graph block-index)
  (setf (gethash block-index (ssa-graph-reachable-blocks graph)) t))

(defun ssa-graph-add-block-successors (graph block-index successors)
  (setf (gethash block-index (ssa-graph-successors graph)) successors))

(defun ssa-graph-get-block-successors (graph block-index)
  (let ((successors (gethash block-index (ssa-graph-successors graph))))
    successors))

(defun ssa-graph-get-block-cond-succ (graph block-index)
  (getf (ssa-graph-get-block-successors graph block-index) :cond))

(defun ssa-graph-get-block-uncond-succ (graph block-index)
  (getf (ssa-graph-get-block-successors graph block-index) :uncond))

(defun ssa-graph-get-block-fall-trough-succ (graph block-index)
  (getf (ssa-graph-get-block-successors graph block-index) :fall-trough))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO, we should use the same SCCP pass for inferring types
(defstruct lattice-value constant type)

(defun default-lattice-value ()
  (make-lattice-value :constant 'TOP))

(defun lattice-meet (l1 l2)
  (error "TODO"))

(defun lattice-value-equal (l1 l2)
  (if (or (lattice-value-p l1)
	  (lattice-value-p l2))
      (error "Still don't suport LATTICE-VALUE")
      (equalp l1 l2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-block-successors (graph b)
  (let ((cond-succ (cssa::ssa-block-cond-jump b))
	(uncond-succ (cssa::ssa-block-uncond-jump b))
	(fall-trough-succ (cssa::ssa-block-succ b)))
    (ssa-graph-add-block-successors graph (cssa::ssa-block-index b)
				    (list :cond cond-succ
					  :uncond uncond-succ
					  :fall-trough fall-trough-succ))))

(defun collect-ssa-edges-from-block (graph block)
  (declare (optimize debug))
  (dolist (phi (cssa::ssa-block-phis block))
    (unless (cssa::get-phi-place-reduced-value (cssa::phi-place phi))
      (ssa-graph-add-def graph (cssa::phi-place phi) phi)
      (dolist (operand (cssa::phi-operands phi))
	(ssa-graph-add-use graph operand phi))))
  (let ((instructions (cssa::ssa-block-ssa block))
	(block-index (cssa::ssa-block-index block)))
    (dolist (ir instructions)
      (if (cssa::ssa-form-rw-p ir)
	  (let* ((rws (cssa::ssa-form-rw-reads-writes ir))
		 (reads (first rws))
		 (writes (second rws)))
	    ;; we want IR that has READ and WRITE places but also we want SSA-IF
	    (when (and reads (or writes
				 (cssa::ssa-if-p ir)))
	      (print (list 'adding-edges ir))
	      (dolist (write writes)
		(typecase write
		  (cssa::named-place
		   (ssa-graph-add-def graph write ir))))
	      (dolist (read reads)
		(typecase read
		  (cssa::named-place
		   (ssa-graph-add-use graph read ir)))))
	    ;; set block index for instruction
	    (ssa-graph-connect-ir-block graph (cssa::ssa-form-index ir) block-index))
	  (print (list 'collect-ssa-edges-from-block 'skip ir))))))

(defun init-lattice-map (lambda-ssa)
  (declare (optimize debug))
  (let ((lattice-map (make-hash-table)))
    (dolist (b (clcomp.ssa::lambda-ssa-blocks lambda-ssa))
      (dolist (phi (cssa::ssa-block-phis b))
	(unless (cssa::get-phi-place-reduced-value (cssa::phi-place phi))
	  (set-lattice-value (cssa::phi-place phi) 'TOP lattice-map)))
      (dolist (ir (clcomp.ssa::ssa-block-ssa b))
	(typecase ir
	  (cssa::ssa-form-rw 
	   (let* ((reads-writes (clcomp.ssa::ssa-form-rw-reads-writes ir))
		  (writes (second reads-writes )))
	     (dolist (write writes)
	       (when (or (clcomp.ssa::virtual-place-p write)
			 (clcomp.ssa::phi-place-p  write))
		 (assert (null (gethash (get-place write) lattice-map)))
		 (set-lattice-value (get-place write) 'TOP lattice-map))))))))
    lattice-map))

(defun set-lattice-value (place value lattice-map)
  (setf (gethash place lattice-map) value))

(defun maybe-change-lattice (p lattice-value lattice-map)
  (let ((old (gethash p lattice-map)))
    (when (not (lattice-value-equal old lattice-value))
      (set-lattice-value p lattice-value lattice-map))))

(defun is-constant (p)
  (cssa::immediate-constant-p p))

(defun is-nil-constant (p)
  (and (cssa::immediate-constant-p p)
       (= clcomp::*nil* (cssa::immediate-constant-constant p))))

(defun get-lattice-value (p lattice-map)
  (typecase p
    (cssa::immediate-constant p)
    (t
     (gethash p lattice-map))))

(defun init-ssa-graph (lambda-ssa)
  (declare (optimize debug))
  (let ((graph (make-default-graph lambda-ssa)))
    (dolist (b (clcomp.ssa::lambda-ssa-blocks lambda-ssa))
      (collect-ssa-edges-from-block graph b)
      (add-block-successors graph b))
    graph))

(defun meet (a b)
  (if (eq a b)
      a
      (cond ((or (eq 'BOTTOM a)
		 (eq 'BOTTOM b))
	     'BOTTOM)
	    ((or (eq 'TOP a)
		 (eq 'TOP b))
	     (if (eq 'TOP a)
		 b
		 a))
	    ((lattice-value-equal a b) a)
	    (t 'BOTTOM))))

(defun meet-all-values (vals)
  (reduce #'meet vals :initial-value 'TOP))

(defun meet-all (lattice-map places)
  (declare (optimize debug))
  (let ((start 'TOP))
    (dolist (p places)
      (let ((lv (get-lattice-value p lattice-map)))
	(assert lv)
	(setf start (meet start lv))))
    start))

(defun visit-phi (phi ssa-graph lattice-map)
  (declare (optimize debug))
  (let ((phi-block (cssa::phi-block-index phi))
	(current-lattice-value (get-lattice-value (get-place phi) lattice-map))
	(lattice-values nil))
    (assert current-lattice-value)
    (do ((operands (cssa::phi-operands phi) (cdr operands))
	 (pred-blocks (cssa::phi-operands-blocks phi) (cdr pred-blocks)))
	((null operands))
      (let ((operand (first operands))
	    (pred-block (first pred-blocks)))
	(assert (and operand pred-block))
	(let ((edge (cons pred-block phi-block)))
	  ;; only evaluate operand if flow edge is active
	  (if (ssa-graph-get-flow-edge-executable ssa-graph edge)
	      (let ((operand-lattice-value (get-lattice-value (get-place operand) lattice-map)))
		(assert operand-lattice-value)
		(push operand-lattice-value lattice-values))
	      ;; If EDGE is not active lattice value is TOP
	      (push 'TOP lattice-values)))))
    (let ((meet (meet-all-values lattice-values)))
      (if (lattice-value-equal current-lattice-value meet)
	  (values nil nil)
	  (progn
	    (set-lattice-value (get-place phi) meet lattice-map)
	    (values nil
		    (ssa-graph-get-use ssa-graph (get-place phi))))))))

;; FIXME, we need more thinking about this
(defun ir-produce-bottom (ir)
  (typecase ir
    (cssa::ssa-load nil)
    ;; ((or cssa::ir1-ssa-receive-arguments
    ;; 	 cssa::ir1-ssa-fun-call)
    ;;  t)
    (t t)))

(defun visit-expression (ir lattice-map ssa-graph block-index)
  (declare (optimize debug))
  (let ((ssa-work-list nil)
	(flow-edges nil))
    (typecase ir
      (cssa::phi
       (visit-phi ir ssa-graph lattice-map))
      ;; FIXME, we can have SSA-IF but flow-trough will not have SSA-GO after this IF
      (cssa::ssa-if
       (let* ((test-place (cssa::ssa-if-test ir))
	      (test-lattice (get-lattice-value test-place lattice-map)))
	 (assert test-lattice)
	 (let ((cond-succ (ssa-graph-get-block-cond-succ ssa-graph block-index))
	       (uncond-succ (ssa-graph-get-block-uncond-succ ssa-graph block-index))
	       (fall-trough-succ (ssa-graph-get-block-fall-trough-succ ssa-graph block-index)))
	   (setf uncond-succ (or uncond-succ fall-trough-succ))
	   (case test-lattice
	     (TOP
	      ;; do nothing
	      ) 
	     (BOTTOM
	      (when cond-succ
		(push (cons block-index cond-succ) flow-edges))
	      (when uncond-succ
		(push (cons block-index uncond-succ ) flow-edges))) 
	     (otherwise
	      (if (is-nil-constant test-lattice)
		  (when uncond-succ
		    (push (cons block-index uncond-succ ) flow-edges))
		  (when cond-succ
		    (push (cons block-index cond-succ) flow-edges))))))))
      (cssa::ssa-go 
       (let ((uncond-succ (ssa-graph-get-block-uncond-succ ssa-graph block-index))
	     (fall-trough-succ (ssa-graph-get-block-fall-trough-succ ssa-graph block-index)))
	 (assert (not (equal uncond-succ fall-trough-succ)))
	 (push  (cons block-index uncond-succ) flow-edges)))
      (cssa::ssa-form-rw
       (let* ((rws (cssa::ssa-form-rw-reads-writes ir))
	      (reads (first rws))
	      (writes (second rws))
	      (fixed-place-read (some #'cssa::fixed-place-p reads))
	      (produce-bottom (ir-produce-bottom ir))
	      (reads-meet (when reads (meet-all lattice-map reads))))
	 (assert (not fixed-place-read))
	 (dolist (w writes)
	   (assert (cssa::named-place-p w))
	   (if produce-bottom
	       ;; NOTE, we currently don't have any constant-folding metadata for our functions and VOP's
	       ;; so currently all forms beside LOAD will produce BOTTOM
	       (when (maybe-change-lattice w 'BOTTOM lattice-map)
		 (let ((uses (ssa-graph-get-use ssa-graph w)))
		   (setf ssa-work-list uses)))
	       (when (and reads-meet
			  (not (eq reads-meet (get-lattice-value w lattice-map))))
		 (set-lattice-value w reads-meet lattice-map)
		 (let ((uses (ssa-graph-get-use ssa-graph w)))
		   (setf ssa-work-list (append ssa-work-list uses))))))))
      (t (print (list 'visit-expression 'skip ir))))
    (values flow-edges ssa-work-list)))

(defun visit-expression-if-block-reachable (ir lattice-map ssa-graph)
  (declare (optimize debug))
  (let ((block-index (ssa-graph-get-ir-block-index ssa-graph ir)))
    (assert block-index)
    (when (ssa-graph-is-block-reachable ssa-graph block-index)
      (visit-expression ir lattice-map ssa-graph block-index)))) ;; FIXME

(defun maybe-visit-expression (expr lattice-map ssa-graph)
  (etypecase expr
    (cssa::phi (visit-phi expr ssa-graph lattice-map))
    (cssa::ssa-form-rw (visit-expression-if-block-reachable expr lattice-map ssa-graph))))

;; VISIT block will handle creating new FLOW EDGES
(defun visit-block (b lattice-map ssa-graph)
  (declare (optimize debug))
  (let ((block-index (cssa::ssa-block-index b))
	(ssa-work-list nil)
	(flow-edges))
    (ssa-graph-set-block-reachable ssa-graph block-index)
    (dolist (ir (cssa::ssa-block-ssa b))
      (multiple-value-bind (new-edges new-ssa-work-list) (visit-expression ir lattice-map ssa-graph block-index)
	(setf ssa-work-list (append ssa-work-list new-ssa-work-list))
	(setf flow-edges (append flow-edges new-edges))))
    (values flow-edges ssa-work-list)))

;; FIXME, this need to return new SSA-WORK-LIST and FLOW-WORK-LIST
(defun process-flow-edge (edge lattice-map graph)
  (declare (optimize debug))
  (let ((exec-flag (ssa-graph-get-flow-edge-executable graph edge))
	(ssa-work-list nil)
	(flow-work-list nil))
    (unless exec-flag
      (ssa-graph-update-flow-edge-executable graph edge)
      (let ((b (ssa-graph-get-block graph (cdr edge))))
	;; process PHI's
	(dolist (phi (cssa::ssa-block-phis b))
	  (unless (cssa::get-phi-place-reduced-value (cssa::phi-place phi))
	    (multiple-value-bind (_ new-ssa-work-list) (visit-phi phi graph lattice-map)
	      (declare (ignore _))
	      (setf ssa-work-list (append ssa-work-list new-ssa-work-list)))))
	;; process block only if this is first time we arrived to this edge (block)
	(when (= 1 (ssa-graph-get-flow-edge-executable graph edge))
	  (multiple-value-bind (new-edges new-ssa-work-list) (visit-block b lattice-map graph)
	    (setf flow-work-list (append flow-work-list new-edges))
	    (setf ssa-work-list (append ssa-work-list new-ssa-work-list))))))
    (values flow-work-list ssa-work-list)))

(defun do-sccp (lambda-ssa)
  (declare (optimize debug))
  (let* ((ssa-graph (init-ssa-graph lambda-ssa))
	 (lattice-map (init-lattice-map lambda-ssa))
	 (block1 (first (clcomp.ssa::lambda-ssa-blocks lambda-ssa))))

    (multiple-value-bind (flow-work-list ssa-work-list) (visit-block block1 lattice-map ssa-graph)
      (let ((new-flow-work-list nil)
	    (new-ssa-work-list nil))

	(tagbody start
	   (unless (or flow-work-list ssa-work-list)
	     (go end))

	   (dolist (ir ssa-work-list)
	     (multiple-value-bind (edges ssas) (maybe-visit-expression ir lattice-map ssa-graph)
	       (setf new-flow-work-list (append new-flow-work-list edges))
	       (setf new-ssa-work-list (append new-ssa-work-list ssas))))

	   (dolist (flow-edge flow-work-list)
	     (multiple-value-bind (edges ssas) (process-flow-edge flow-edge lattice-map ssa-graph)
	       (setf new-flow-work-list (append new-flow-work-list edges))
	       (setf new-ssa-work-list (append new-ssa-work-list ssas))))

	   (setf flow-work-list new-flow-work-list)
	   (setf new-flow-work-list nil)

	   (setf ssa-work-list new-ssa-work-list)
	   (setf new-ssa-work-list nil)

	   (go start)

	 end)))
    ;; (maphash (lambda (k v)
    ;; 	       (print (list k v)))
    ;; 	     lattice-map)
    ;; (maphash (lambda (k v)
    ;; 	       (print (list 'flow-edges k v)))
    ;; 	     (ssa-graph-flow-edges ssa-graph))
    ;; (maphash (lambda (k v)
    ;; 	       (print (list 'reachable-blocks k v)))
    ;; 	     (ssa-graph-reachable-blocks ssa-graph))
    ))


#+nil
(clcomp-compile nil '(lambda (a)
		      (let ((x 1)
			    (y 1)
			    (r nil))
			(if a
			    (setf r x)
			    (setf r y))
			r
			)))

;; FIXME, we don't follow successor blocks
