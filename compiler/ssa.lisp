(in-package :clcomp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (debug 3) (safety 3) (speed 0))))

(defstruct immediate-constant constant)

(defstruct place)
(defstruct (named-place (:include place)) name)

(defstruct (fixed-place (:include place)))
(defstruct (arg-place (:include fixed-place)) index)

(defstruct (rcv-argument-place (:include arg-place)))
(defstruct (argument-place (:include arg-place)))
(defstruct (return-value-place (:include fixed-place)) index)
(defstruct (argument-count-place (:include fixed-place)))
(defstruct (function-value-place (:include fixed-place)))

(defstruct (virtual-place (:include named-place)))
(defstruct (phi-place (:include named-place)))
(defstruct (var-place (:include named-place)))

(defstruct (fixup (:include named-place)))
(defstruct (local-component-fixup (:include fixup)))
(defstruct (compile-function-fixup (:include fixup)) function)
(defstruct (load-time-eval-fixup (:include fixup)))
(defstruct (compile-time-constant-fixup (:include fixup)) form)

(defstruct slabels alist)
(defstruct ssa-env labels)
(defstruct lambda-ssa blocks blocks-index (env (make-ssa-env)) fixups sub-lambdas (phi-connections (make-hash-table))
  loop-header-block loop-end-blocks (redundant-phis (make-hash-table :test #'equalp)))

(defstruct ssa-block index order ir ssa succ cond-jump uncond-jump predecessors is-loop-end is-header (branch-to-count 0)
  sealed processed label defined phis live-in virtuals live-gen live-kill live-out)

(defstruct ssa-form index)
(defstruct (lambda-entry (:include ssa-form)))
(defstruct (arg-check (:include ssa-form)) arg-count)
(defstruct (ssa-form-rw (:include ssa-form)))

(defstruct (ssa-load (:include ssa-form-rw)) to from)
(defstruct (ssa-go (:include ssa-form)) label)
(defstruct (ssa-label (:include ssa-form)) label)
;;; Do we need SSA-VALUE ??
(defstruct (ssa-value (:include ssa-form-rw)) value)

(defstruct (ssa-base-return (:include ssa-form)))
(defstruct (ssa-return (:include ssa-base-return)))
(defstruct (ssa-multiple-return (:include ssa-base-return)))

(defstruct (ssa-vop (:include ssa-form-rw)) tos froms)

;;; FIXME, still not sure when to use which one
(defstruct (ssa-fun-call (:include ssa-form)) fun)
(defstruct (ssa-unknown-values-fun-call (:include ssa-fun-call)))
(defstruct (ssa-known-values-fun-call (:include ssa-fun-call)) min-values)

(defstruct (ssa-if (:include ssa-form-rw)) test true-block)

(defparameter *instr-offset* 2)
(defparameter *debug-stream* t)

(defparameter *gen-symbol-counter* 0)

(defparameter *block-header-counter* 0)
(defun get-block-unique-header-number ()
  (prog1 *block-header-counter*
    (incf *block-header-counter*)))

(defparameter *ssa-symbol-counter* 0)
(defun generate-virtual-place (&optional (s "T-"))
  (prog1
      (make-virtual-place :name (make-symbol (concatenate 'string s (write-to-string *ssa-symbol-counter*))))
    (incf *ssa-symbol-counter*)))

(defparameter *if-label-counter* 0)
(defun generate-if-label-symbol ()
  (prog1
      (make-symbol (concatenate 'string "IF-NEXT-BLOCK-LABEL-" (write-to-string *ssa-symbol-counter*)))
    (incf *ssa-symbol-counter*)))

(defun generate-label-symbol ()
  (prog1
      (make-symbol (concatenate 'string "REPLACE-LABEL-" (write-to-string *ssa-symbol-counter*)))
    (incf *ssa-symbol-counter*)))

(defparameter *ssa-block-counter* 0)
(defun make-new-ssa-block ()
  (prog1
      (make-ssa-block :index *ssa-block-counter*)
    (incf *ssa-block-counter*)))

(defparameter *fixup-symbol-counter* 0)
(defun generate-fixup-symbol ()
  (prog1
      (make-symbol (concatenate 'string "FIXUP-" (write-to-string *fixup-symbol-counter*)))
    (incf *fixup-symbol-counter*)))

(defparameter *phi-place-symbol-counter* 0)
(defun generate-phi-place ()
  (let ((symbol (make-symbol (concatenate 'string "PHI-PLACE-" (write-to-string *phi-place-symbol-counter*)))))
    (prog1 (make-phi-place :name symbol)
      (incf *phi-place-symbol-counter*))))

(defun lambda-add-fixup (fixup lambda-ssa)
  (push fixup (lambda-ssa-fixups lambda-ssa)))

(defun ssa-block-add-phi (ssa-block phi)
  (setf (ssa-block-phis ssa-block) (acons (named-place-name (phi-place phi)) phi
					  (ssa-block-phis ssa-block))))

(defun ssa-block-replace-phi (ssa-block phi-place new-value)
  (let ((cons (assoc (named-place-name phi-place) (ssa-block-phis ssa-block))))
    (setf (cdr cons) new-value)))

(defun ssa-block-get-maybe-phi (ssa-block phi-place)
  (cdr (assoc (named-place-name phi-place) (ssa-block-phis ssa-block))))

(defun ssa-block-is-place-phi (ssa-block phi-place)
  (phi-p (ssa-block-get-maybe-phi ssa-block phi-place)))

(defun add-phi-connections (phi operand lambda-ssa)
  (unless (equalp (phi-place phi) operand)
    (let ((name (named-place-name operand)))
      (push phi (gethash name (lambda-ssa-phi-connections lambda-ssa))))))

(defun get-phi-connections (phi-place lambda-ssa)
  (gethash (named-place-name phi-place) (lambda-ssa-phi-connections lambda-ssa)))

(defun add-phi-value-replacement (phi-place value lambda-ssa)
  (setf (gethash phi-place (lambda-ssa-redundant-phis lambda-ssa)) value))

(defun get-phi-value-replacement (phi-place lambda-ssa)
  (gethash phi-place (lambda-ssa-redundant-phis lambda-ssa)))

(defun ssa-block-all-phis (ssa-block)
  (mapcar #'cdr (ssa-block-phis ssa-block)))

(defun ssa-add-block (lambda-ssa block)
  (push block (lambda-ssa-blocks lambda-ssa)))

(defun ssa-connect-blocks (b1 b2)
  (if (ssa-block-succ b1)
      (error "Block already have successor !")
      (setf (ssa-block-succ b1) (ssa-block-index b2))))

(defun ssa-maybe-connect-blocks (b1 b2)
  (unless (ssa-block-uncond-jump b1)
    (ssa-connect-blocks b1 b2)))

(defun insert-block-conditional-jump (block jump-block-index)
  (setf (ssa-block-cond-jump block) jump-block-index))

(defun insert-block-unconditional-jump (block jump-block-index)
  (if (ssa-block-uncond-jump block)
      (error "Unconditional jump already exist !"))
  (setf (ssa-block-uncond-jump block) jump-block-index))

(defun pop-labels-env (lambda-ssa)
  (let ((ssa-env (lambda-ssa-env lambda-ssa)))
    (pop (ssa-env-labels ssa-env))))

(defun push-labels-env (lambda-ssa)
  (let ((ssa-env (lambda-ssa-env lambda-ssa)))
    (push (make-slabels) (ssa-env-labels ssa-env))))

(defun label-ssa-block (block label)
  (setf (ssa-block-label block) label))

(defun ssa-add-block-label (lambda-ssa block label)
  (let* ((env (lambda-ssa-env lambda-ssa))
	 (slabels (first (ssa-env-labels env))))
    (label-ssa-block block label)
    (push (cons label (ssa-block-index block))
	  (slabels-alist slabels))))

(defun ssa-find-block-index-by-label (lambda-ssa label)
  (let* ((env (lambda-ssa-env lambda-ssa))
	 (slabels (ssa-env-labels env)))
    (dolist (slabel slabels)
      (let ((cons (assoc label (slabels-alist slabel))))
	(when cons
	  (return-from ssa-find-block-index-by-label (cdr cons)))))))

(defun ssa-find-block-by-index (lambda-ssa index)
  (gethash index (lambda-ssa-blocks-index lambda-ssa)))

(defun blocks-have-same-index (b1 b2)
  (= (ssa-block-index b1)
     (ssa-block-index b2)))

(defun add-sub-lambda (lambda-ssa sub-lambda-ssa fixup-symbol)
  (push (cons fixup-symbol sub-lambda-ssa)
	(lambda-ssa-sub-lambdas lambda-ssa)))

(defparameter *ir-index-counter* 0)
(defun emit (lambda-ssa ssa block)
  (declare (ignore lambda-ssa))
  (when (ssa-form-p ssa)
    (setf (ssa-form-index ssa) *ir-index-counter*)
    (incf *ir-index-counter*))
  (push ssa (ssa-block-ir block)))

(defun emit-first (lambda-ssa ssa block)
  (declare (ignore lambda-ssa))
  (when (ssa-form-p ssa)
    (setf (ssa-form-index ssa) *ir-index-counter*)
    (incf *ir-index-counter*))
  (setf (ssa-block-ir block)
	(cons ssa (ssa-block-ir block))))

(defun ssa-block-last-instruction (block)
  (car (last (ssa-block-ssa block))))

(defun ssa-block-first-instruction (block)
  (car (ssa-block-ssa block)))

(defun ssa-block-first-index (block)
  (ssa-form-index (ssa-block-first-instruction block)))

(defun ssa-block-last-index (block)
  (ssa-form-index (ssa-block-last-instruction block)))

(defun is-last-instr-jump (block)
  (and (ssa-go-p (ssa-block-last-instruction block))
       (ssa-block-last-instruction block)))

(defun is-first-instr-label (block)
  (and (ssa-label-p (ssa-block-first-instruction block))
       (ssa-block-first-instruction block)))

(defun remove-last-instruction (block)
  (setf (ssa-block-ssa block) (butlast (ssa-block-ssa block))))

(defun emit-single-return-sequence (lambda-ssa place block)
  (emit lambda-ssa (make-ssa-load :to (make-return-value-place :index 0) :from place) block)
  (emit lambda-ssa (make-ssa-return) block))

;;; FIXME, set block LABEL fieds
(defun emit-if-node-ssa (if-node lambda-ssa leaf place block)
  (let* ((test-node (if-node-test-form if-node))
	 (test-place (generate-virtual-place))
	 (block (emit-ssa test-node lambda-ssa nil test-place block))
	 (false-block (make-new-ssa-block))
	 (true-block (make-new-ssa-block))
	 (next-block (unless leaf (make-new-ssa-block))))
    ;; FIXME, can happen that true-block and next-block are not near each other
    ;; so we need to insert JUMP here
    ;; (unless leaf
    ;;   (ssa-connect-blocks true-block next-block))
    (ssa-connect-blocks block false-block)
    (ssa-add-block lambda-ssa false-block)
    (ssa-add-block lambda-ssa true-block)
    (let ((true-form-ret-block (emit-ssa (if-node-true-form if-node)
					 lambda-ssa leaf place true-block))
	  (false-form-ret-block (emit-ssa
				 (if-node-false-form if-node)
				 lambda-ssa leaf place false-block)))
      (unless leaf
	(if (blocks-have-same-index true-block true-form-ret-block)
	    (ssa-maybe-connect-blocks true-block next-block)
	    (ssa-maybe-connect-blocks true-form-ret-block next-block))
	(if (blocks-have-same-index false-block false-form-ret-block)
	    (insert-block-unconditional-jump false-block (ssa-block-index next-block))
	    (insert-block-unconditional-jump false-form-ret-block (ssa-block-index next-block)))))
    ;; it's important to add next-block after emiting IR for false and true blocks
    ;; if any of false/true blocks creates new blocks than we keep good block order
    (unless leaf
      (ssa-add-block lambda-ssa next-block))
    (insert-block-conditional-jump block (ssa-block-index true-block))
    (emit lambda-ssa
	  (make-ssa-if
	   :test test-place
	   :true-block (ssa-block-index true-block))
	  block)
    next-block))

(defun maybe-emit-direct-load (node lambda-ssa leaf place block)
  (etypecase node
    (immediate-constant-node
     (emit lambda-ssa (make-ssa-load :to place
    				     :from node) block)
     (when leaf
       (emit-single-return-sequence lambda-ssa place block))
     block)
    (lexical-var-node
     (emit lambda-ssa (make-ssa-load :to place
    				     :from (make-var-place :name (lexical-var-node-name node))) block)
     (when leaf
       (emit-single-return-sequence lambda-ssa place block))
     block)
    (t (emit-ssa node lambda-ssa leaf place block))))

;;; FIXME, there is more here
(defun make-direct-place-or-nil (node)
  (etypecase node
    (immediate-constant-node node)
    (lexical-var-node (make-var-place :name (lexical-var-node-name node)))
    (t nil)))

;;; FIXME, fun can be CLOSURE or LAMBDA
(defun emit-call-node-ssa (node lambda-ssa leaf place block)
  (let* ((fun (call-node-function node))
	 (arguments (call-node-arguments node))
	 (arguments-count (length arguments))
	 (args-places nil))
    (dolist (arg arguments)
      (let ((direct-place (make-direct-place-or-nil arg)))
	(if direct-place
	    (push direct-place args-places)
	    (let* ((place (generate-virtual-place))
		   (new-block (maybe-emit-direct-load arg lambda-ssa nil place block)))
	      (setf block new-block)
	      (push place args-places)))))
    (let ((arg-index 0))
      (dolist (p (reverse args-places))
	(emit lambda-ssa (make-ssa-load :to (make-argument-place :index arg-index) :from p) block)
	(incf arg-index)))
    (emit lambda-ssa (make-ssa-load :to (make-argument-count-place)
				    :from (make-immediate-constant :constant (fixnumize arguments-count))) block)
    (let ((fixup (make-compile-function-fixup :name (generate-fixup-symbol)
					      :function fun)))
      (emit lambda-ssa (make-ssa-load :to (make-function-value-place) :from fixup) block)
      (lambda-add-fixup fixup lambda-ssa))
    (if (null place)
	(progn
	  (emit lambda-ssa
		(make-ssa-unknown-values-fun-call :fun fun)
		block)
	  (when leaf
	    (emit lambda-ssa (make-ssa-multiple-return) block))
	  block)
	(progn
	  (emit lambda-ssa (make-ssa-known-values-fun-call :fun fun :min-values 1) block)
	  (emit lambda-ssa (make-ssa-load :to place :from (make-return-value-place :index 0)) block)
	  block))))

(defun emit-lexical-binding-node-ssa (node lambda-ssa leaf block)
  (declare (ignore leaf))
  (let ((lvar (lexical-binding-node-name node))
	(form (lexical-binding-node-form node)))
    (maybe-emit-direct-load form lambda-ssa nil (make-var-place :name lvar) block)))

(defun emit-let-node-ssa (node lambda-ssa leaf place block)
  (dolist (n (let-node-bindings node))
    (let ((new-block (emit-lexical-binding-node-ssa n lambda-ssa nil block)))
      (setf block new-block)))
  (emit-ssa (let-node-form node) lambda-ssa leaf place block))

(defun emit-immediate-node-ssa (node lambda-ssa leaf place block)
  (if place
      (emit lambda-ssa (make-ssa-load :to place :from node) block)
      (if leaf
	  (emit-single-return-sequence lambda-ssa node block)
	  (emit lambda-ssa (make-ssa-value :value node) block)))
  block)

(defun emit-lexical-var-node-ssa (node lambda-ssa leaf place block)
  (if place
      (emit lambda-ssa (make-ssa-load :to place :from (make-var-place :name (lexical-var-node-name node))) block )
      (if leaf
	  (emit-single-return-sequence lambda-ssa (make-var-place :name (lexical-var-node-name node))  block)
	  (emit lambda-ssa (make-ssa-value :value
					   (make-var-place :name (lexical-var-node-name node))) block)))
  block)

(defun emit-progn-node-ssa (node lambda-ssa leaf place block)
  (do* ((forms (progn-node-forms node) (cdr forms))
	(form-node (car forms) (car forms)))
       ((null forms) block)
    (if (cdr forms)
	(setf block (emit-ssa form-node lambda-ssa nil nil block))
	(setf block (emit-ssa form-node lambda-ssa leaf place block)))))

(defun emit-tagbody-node-ssa (node lambda-ssa leaf place block)
  (push-labels-env lambda-ssa)
  (let ((labels-blocks (make-hash-table))
	(current-block block))
    (dolist (node (tagbody-node-forms node))
      (when (label-node-p node)
	(let ((label-block (make-new-ssa-block)))
	  (ssa-add-block-label lambda-ssa label-block (label-node-label node))
	  (setf (gethash (label-node-label node) labels-blocks) label-block))))
    (dolist (form-node (tagbody-node-forms node))
      (if (label-node-p form-node)
	  (let ((lblock (gethash (label-node-label form-node) labels-blocks)))
	    ;; connect previus block with this label block only if there is no direct jump from previous to other block
	    (ssa-add-block lambda-ssa lblock)
	    ;; when current block doesn't have GO (it never should thought)
	    ;; just connect blocks
	    (when (null (ssa-block-uncond-jump current-block))
	      (ssa-connect-blocks current-block lblock))
	    (emit lambda-ssa
		  (make-ssa-label :label (label-node-label form-node))
		  lblock)
	    (setf current-block lblock))
	  (setf current-block (emit-ssa form-node lambda-ssa nil nil current-block)))
      (setf block current-block))
    (pop-labels-env lambda-ssa)
    (if place
	(emit lambda-ssa (make-ssa-load :to place :from (make-immediate-constant :constant *nil*)) block)
	(when leaf
	  (emit-single-return-sequence lambda-ssa (make-immediate-constant :constant *nil*) block )
	  ;; FIXME, need this ?
	  ;; (emit lambda-ssa (make-immediate-constant :constant *nil*) block)
	  ))
    current-block))

;;; FIXME, delete unreachable code
;;; FIXME, dead code after go will be recorded in new SSA-BLOCK that will be lost without notification
(defun emit-go-node-ssa (node lambda-ssa leaf place block)
  (declare (ignore leaf place))
  (let ((label-name (label-node-label (go-node-label-node node)) ))
    (insert-block-unconditional-jump block (ssa-find-block-index-by-label lambda-ssa label-name))
    (emit lambda-ssa (make-ssa-go :label label-name) block)
    ;; reset successor to nil, if we have GO direct successor in this block is never active
    (setf (ssa-block-succ block) nil)
    (make-new-ssa-block)))

(defun emit-setq-node-ssa (node lambda-ssa leaf place block)
  (let ((new-block (maybe-emit-direct-load (setq-node-form node)
					   lambda-ssa leaf
					   (make-var-place
					    :name (lexical-var-node-name
						   (setq-node-var node)))
					   block)))
    (if place
	(maybe-emit-direct-load (setq-node-var node) lambda-ssa leaf place new-block))
    new-block))

(defun emit-lambda-arguments-ssa (arguments lambda-ssa block)
  (emit lambda-ssa (make-arg-check :arg-count (length arguments)) block)
  (let ((index 0))
    (dolist (argument arguments)
      (etypecase argument
	(lexical-var-node
	 (emit lambda-ssa (make-ssa-load :to (make-var-place :name (lexical-var-node-name argument))
					 :from (make-rcv-argument-place :index index))
	       block)))
      (incf index))))

(defun rip-relative-node-to-fixup (node)
  (let ((fixup-sym (generate-fixup-symbol)))
    (etypecase node
      (lambda-node (make-local-component-fixup :name fixup-sym))
      (load-time-value-node (make-load-time-eval-fixup :name fixup-sym))
      (fun-rip-relative-node (make-compile-function-fixup :name fixup-sym
							  :function (fun-rip-relative-node-form node)))
      (compile-time-constant-node (make-compile-time-constant-fixup :name fixup-sym
								    :form (compile-time-constant-node-form node))))))

(defun emit-rip-relative-node-ssa (node lambda-ssa leaf place block)
  (let ((fixup (rip-relative-node-to-fixup node)))
    (typecase node
      (load-time-value-node
       (add-sub-lambda lambda-ssa (lambda-construct-ssa (load-time-value-node-node node)) fixup))
      (lambda-node
       (add-sub-lambda lambda-ssa (lambda-construct-ssa node) fixup)))
    (lambda-add-fixup fixup lambda-ssa)
    (if place
	(emit lambda-ssa (make-ssa-load :to place :from fixup) block)
	(if leaf
	    (emit-single-return-sequence lambda-ssa fixup block)
	    (emit lambda-ssa (make-ssa-value :value fixup) block)))
    block))

;;; FIXME, don't macroexpand BLOCK to TAGBODY, implement BLOCK directly in IR
(defun emit-ssa (node lambda-ssa leaf place block)
  (etypecase node
    (if-node (emit-if-node-ssa node lambda-ssa leaf place block))
    (call-node (emit-call-node-ssa node lambda-ssa leaf place block))
    (vop-node (emit-call-node-ssa node lambda-ssa leaf place block))
    (let-node (emit-let-node-ssa node lambda-ssa leaf place block))
    (progn-node (emit-progn-node-ssa node lambda-ssa leaf place block))
    (lexical-var-node (emit-lexical-var-node-ssa node lambda-ssa leaf place block))
    (immediate-constant-node (emit-immediate-node-ssa node lambda-ssa leaf place block))
    (tagbody-node (emit-tagbody-node-ssa node lambda-ssa leaf place block))
    (go-node (emit-go-node-ssa node lambda-ssa leaf place block))
    (setq-node (emit-setq-node-ssa node lambda-ssa leaf place block))
    (rip-relative-node (emit-rip-relative-node-ssa node lambda-ssa leaf place block))))

;;; check all jumps/successors on blocks and reorder if necessary
;; FIXME, for now we need to check if JUMP form is last block instruction
;; maybe it's not if dead code exists
;; dead code can't exist
(defun maybe-remove-uncond-jump (b1 b2)
  (when (and b2
	     (= (ssa-block-uncond-jump b1) (ssa-block-index b2))
	     (is-last-instr-jump b1))
    (setf (ssa-block-succ b1) (ssa-block-uncond-jump b1))
    (setf (ssa-block-uncond-jump b1) nil)
    (remove-last-instruction b1)
    (format *debug-stream* "Reseting block unconditional jump to NIL, setting successor to ~A~%" (ssa-block-succ b1))))

(defun maybe-insert-jump-instruction-for-blocks (b1 b2 lambda-ssa)
  (unless b2
    (error "Successor block in NIL"))
  (let ((label-instr (is-first-instr-label b2)))
    (unless label-instr
      (setf label-instr (make-ssa-label :label (generate-label-symbol)))
      (emit-first lambda-ssa label-instr b2)
      (label-ssa-block b2 (ssa-label-label label-instr)))	
    (emit lambda-ssa (make-ssa-go :label (ssa-label-label label-instr)) b1)))

(defun maybe-insert-jump-for-successor (b1 next-block lambda-ssa)
  (let ((b1-succ (ssa-block-succ b1))
	(next-block-index (and next-block (ssa-block-index next-block))))
    (when (or (null next-block-index)
	      (/= b1-succ next-block-index))
      (insert-block-unconditional-jump b1 b1-succ)
      (setf (ssa-block-succ b1) nil)
      (format *debug-stream* "Changing block ~A successor to uncond-jump to ~A~%" (ssa-block-index b1) b1-succ)
      (maybe-insert-jump-instruction-for-blocks b1 (ssa-find-block-by-index lambda-ssa b1-succ) lambda-ssa))))

(defun ssa-maybe-fix-blocks-connections (lambda-ssa)
  (let ((blocks (lambda-ssa-blocks lambda-ssa)))
    (do* ((bs blocks (cdr bs))
	  (b1 (first bs) (first bs))
	  (b2 (second bs) (second bs)))
	 ((null bs))
      (let ((b1-succ (ssa-block-succ b1))
	    ;; FIXME, if cond-jump BLOCK is just JUMP instruction then cond-jump BLOCK can be deleted
	    ;; and JUMP target just can replace this block JUMP
	    ;; (b1-cond-jump (ssa-block-cond-jump b1))
	    (b1-uncond-jump (ssa-block-uncond-jump b1)))
	(when b1-succ
	  (maybe-insert-jump-for-successor b1 b2 lambda-ssa))
	(when b1-uncond-jump
	  (maybe-remove-uncond-jump b1 b2))))))


(defun ssa-normalize-lambda-ssa (lambda-ssa)
  (let ((blocks nil))
    (dolist (b (lambda-ssa-blocks lambda-ssa))
      (setf (ssa-block-ir b)
	    (reverse (ssa-block-ir b)))
      (push b blocks))
    (setf (lambda-ssa-blocks lambda-ssa) blocks)
    (let ((ir-index 0))
      (dolist (b blocks)
	;; reindex instruction
	(dolist (ir (ssa-block-ir b))
	  (setf (ssa-form-index ir) ir-index)
	  (incf ir-index *instr-offset*)))))
  lambda-ssa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; value numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct phi operands incomplete variable place block-index)

(defun phi-add-operand (phi operand lambda-ssa)
  (when (phi-place-p operand)
    (add-phi-connections phi operand lambda-ssa))
  (etypecase operand
    ((or virtual-place phi-place)
     (push operand (phi-operands phi)))))

(defun get-block-def (block symbol)
  (cdr (assoc symbol (ssa-block-defined block))))

(defun set-block-def (block var value)
  (let ((cons (assoc var (ssa-block-defined block))))
    (if cons
	(setf (cdr cons) value)
	(setf (ssa-block-defined block)
	      (acons var value (ssa-block-defined block))))
    value))

(defun replace-phi-operand (phi old new)
  (let (newops)
    (dolist (op (phi-operands phi))
      (if (equalp op old)
	  (push new newops)
	  (push op newops)))
    (setf (phi-operands phi)
	  newops)))

(defun fill-blocks-predecessors (lambda-ssa)
  (dolist (block (lambda-ssa-blocks lambda-ssa))
    (let ((block-index (ssa-block-index block))
	  (succ (ssa-block-succ block))
	  (uncond-jump (ssa-block-uncond-jump block))
	  (cond-jump (ssa-block-cond-jump block)))
      (when (and succ uncond-jump)
	(error "Can't have successor and unconditioned jump"))
      (dolist (bindex (remove nil (list succ uncond-jump cond-jump)))
	(let ((sblock (ssa-find-block-by-index lambda-ssa bindex)))
	  (push block-index (ssa-block-predecessors sblock)))))))

(defun ssa-write-variable (place block env)
  (declare (ignore env))
  (let ((vplace (generate-virtual-place "V-")))
    (set-block-def block (named-place-name place) vplace)))

(defun try-remove-trivial-phi (phi block lambda-ssa)
  (let ((same nil)
	(phi-place (phi-place phi)))
    (dolist (operand (phi-operands phi))
      (cond ((or (eql operand same)
		 (eq operand (phi-place phi))))
	    ((not (null same))
	     (return-from try-remove-trivial-phi phi))
	    (t (setf same operand))))
    (ssa-block-replace-phi block phi-place same)
    (let ((phi-usages (get-phi-connections phi-place lambda-ssa)))
      (when phi-usages
	(dolist (uphi phi-usages)
	  (replace-phi-operand uphi phi-place same)
	  (try-remove-trivial-phi uphi
				  (ssa-find-block-by-index lambda-ssa (phi-block-index uphi))
				  lambda-ssa))))
    same))

(defun phi-add-operands (place phi predecessors block lambda-ssa)
  (dolist (pblock predecessors)
    (phi-add-operand phi
		     (ssa-read-variable place (ssa-find-block-by-index lambda-ssa pblock) lambda-ssa)
		     lambda-ssa))
  (try-remove-trivial-phi phi block lambda-ssa))

(defun seal-block (ssa-block lambda-ssa)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((phis (ssa-block-all-phis ssa-block)))
    (dolist (phi phis)
      (when (phi-incomplete phi)
	(setf (phi-incomplete phi) nil)
	(phi-add-operands (phi-variable phi) phi (ssa-block-predecessors ssa-block) ssa-block lambda-ssa)))
    (setf (ssa-block-sealed ssa-block) t)))

(defun read-variable-recursive (place block lambda-ssa)
  (let ((predecessors (ssa-block-predecessors block)))
    (if (not (ssa-block-sealed block))
	(let* ((phi-place (generate-phi-place))
	       (phi (make-phi :incomplete t :variable place
			      :place phi-place :block-index (ssa-block-index block))))
	  (ssa-block-add-phi block phi)
	  (set-block-def block (named-place-name place) phi-place))
	(when predecessors
	  (if (= (length predecessors) 1)
	      (set-block-def block (named-place-name place)
			     (ssa-read-variable place (ssa-find-block-by-index lambda-ssa (first predecessors)) lambda-ssa))
	      (let* ((phi-place (generate-phi-place))
		     (phi (make-phi :variable place :place phi-place
				    :block-index (ssa-block-index block))))
		(ssa-block-add-phi block phi)
		(set-block-def block (named-place-name place) phi-place)
		(phi-add-operands place phi predecessors block lambda-ssa)
		phi-place ))))))

(defun ssa-read-variable (place block lambda-ssa)
  (or (get-block-def block (named-place-name place)) 
      (read-variable-recursive place block lambda-ssa)))

(defun transform-write (place block lambda-ssa)
  (typecase place
    (named-place
     (ssa-write-variable place block lambda-ssa))
    (t place)))

(defun transform-read  (place block lambda-ssa)
  (typecase  place
    (fixup place)
    (named-place (ssa-read-variable place block lambda-ssa))
    (t place)))

(defun ssa-transform-block-ir (b lambda-ssa)
  (let ((ssa-ir nil))
    (dolist (ir (ssa-block-ir b))
      (let ((irssa (etypecase ir
		     (ssa-load (make-ssa-load :index (ssa-load-index ir)
					      :to (transform-write (ssa-load-to ir) b lambda-ssa)
					      :from (transform-read (ssa-load-from ir) b lambda-ssa)))
		     (ssa-value (make-ssa-value :index (ssa-value-index ir)
						:value (transform-read (ssa-value-value ir) b lambda-ssa)))
		     (ssa-if (make-ssa-if :index (ssa-if-index ir)
					  :test (transform-read (ssa-if-test ir) b lambda-ssa)
					  :true-block (ssa-if-true-block ir)))
		     (t ir))))
	(push irssa ssa-ir)))
    (setf (ssa-block-ssa b) (reverse ssa-ir))))

(defun do-value-numbering (lambda-ssa)
  (let ((*ssa-symbol-counter* 0))
    (dolist (b (lambda-ssa-blocks lambda-ssa))
      (ssa-transform-block-ir b lambda-ssa)
      (seal-block b lambda-ssa)))
  lambda-ssa)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remove not needed PHI in a case of irreducible loop

(defun sccs-collect (index successors-map visited)
  (unless (gethash index visited)
    (setf (gethash index visited) t)
    (let ((r nil))
      (dolist (i (gethash index successors-map))
	(setf r (append (sccs-collect i successors-map visited) r)))
      (cons index r))))

(defun dfs-get-order (index successors-map visited res)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (unless (gethash index visited)
    (setf (gethash index visited) t)
    (let* ((succ-indexes (gethash index successors-map)))
      (dolist (i succ-indexes)
	(dfs-get-order i successors-map visited res))
      (push index (car res)))))

(defun transpose-graph (indexes successors-map)
  (let ((transposed-successors-map (make-hash-table)))
    (dolist (index indexes)
      (dolist (succ-index (gethash index successors-map))
	(push index (gethash succ-index transposed-successors-map))))
    transposed-successors-map))

(defun compute-phi-sccs (indexes successors-map)
  (let* ((result (cons nil nil))
	 (_ (dfs-get-order (first indexes) successors-map (make-hash-table) result))
	 (order (car result))
	 (transposed-successors (transpose-graph indexes successors-map))
	 (visited (make-hash-table)))
    (declare (ignore _))
    (let ((sccs nil))
      (dolist (i order)
	(when (not (gethash i visited))
	  (push (sccs-collect i transposed-successors visited) sccs)))
      sccs)))

(defun collect-scc-phis (scc lambda-ssa)
  (let ((phis nil))
    (dolist (bindex scc)
      (setf phis (append phis
			 (ssa-block-all-phis (ssa-find-block-by-index lambda-ssa bindex)))))
    phis))

(defun phi-operand-is-in-scc-in-block-phis (operand block)
  (when (phi-place-p operand)
    (dolist (cons (ssa-block-phis block))
      (when (equalp operand (phi-place (cdr cons)) )
	(return t)))))

;;; FIXME, we are looking in both write/read virtuals
;;; we should only look at writes
(defun phi-operand-is-in-scc (operand scc lambda-ssa)
  (dolist (index scc)
    (let ((block (ssa-find-block-by-index lambda-ssa index)))
      (when (or
	     ;; don't look at read virtuals
	     ;; (find operand (first (ssa-block-virtuals block)) :test 'equalp)
	     (find operand (second (ssa-block-virtuals block)) :test 'equalp)
	     (phi-operand-is-in-scc-in-block-phis operand block))
	(return t)))))

(defun collect-redundant-phis-for-scc (scc lambda-ssa)
  (let ((phis (collect-scc-phis scc lambda-ssa))
	(this-scc-phis (make-hash-table :test #'equalp))
	(foperand-phis (make-hash-table :test #'equalp)))
    (dolist (phi phis)
      (when (phi-p phi)
       (when (not (every (lambda (operand)
			   (phi-operand-is-in-scc operand scc lambda-ssa))
			 (phi-operands phi)))
	 (setf (gethash (phi-place phi) this-scc-phis) phi))))
    (dolist (phi (hash-values this-scc-phis))
      (let ((operands (phi-operands phi)))
	;; FIXME, we can have more than 2 operands ?
	(when (= 2 (length operands))
	  (let ((phi-operand nil)
		(foreign-operand nil))
	    (dolist (op operands)
	      (when (and (phi-place-p op)
			 (gethash op this-scc-phis))
		(setf phi-operand op))
	      (when (not (phi-operand-is-in-scc op scc lambda-ssa))
		(setf foreign-operand op)))
	    (when (and phi-operand foreign-operand)
	      (push phi (gethash foreign-operand foperand-phis)))))))
    foperand-phis))

(defun replace-redundant-phis (value phis lambda-ssa)
  (dolist (phi phis)
    (let* ((block-index (phi-block-index phi))
	   (block (ssa-find-block-by-index lambda-ssa block-index)))
      (ssa-block-replace-phi block (phi-place phi) value)
      (add-phi-value-replacement (phi-place phi) value lambda-ssa))))

(defun remove-redundant-phis (lambda-ssa)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((successors-map (make-hash-table))
	(indexes nil))
    (dolist (block (lambda-ssa-blocks lambda-ssa))
      (let ((index (ssa-block-index block))
	    (successors (ssa-block-successors-indexes block)))
	(push index indexes)
	(setf (gethash index successors-map) successors)))
    (let ((sccs (compute-phi-sccs (reverse indexes) successors-map)))
      (dolist (scc sccs)
	(let ((redundant-phis (collect-redundant-phis-for-scc scc lambda-ssa)))
	  (dolist (value (hash-keys redundant-phis))
	    (replace-redundant-phis value (gethash value redundant-phis) lambda-ssa)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun every-predecessor-processed-p (predecessor-indexes lambda-ssa)
  (dolist (pblock predecessor-indexes)
    (let ((block (ssa-find-block-by-index lambda-ssa pblock)))
      (unless (ssa-block-processed block)
	(return-from every-predecessor-processed-p nil))))
  t)

(defun maybe-seal-block (ssa-block lambda-ssa)
  (unless (ssa-block-sealed ssa-block)
    (when (every-predecessor-processed-p (ssa-block-predecessors ssa-block) lambda-ssa)
      (seal-block ssa-block lambda-ssa))))

;;; FIXME, RECURSIVE, possible stack overflow in compiler when it's built
(defun ssa-block-do-construction (ssa-block lambda-ssa)
  (declare (optimize (debug 3) (safety 3) (speed 3)))
  (maybe-seal-block ssa-block lambda-ssa)
  (unless (ssa-block-processed ssa-block)
    (ssa-transform-block-ir ssa-block lambda-ssa)
    (setf (ssa-block-processed ssa-block) t)
    (dolist (sblock (ssa-block-successors ssa-block lambda-ssa))
      (ssa-block-do-construction sblock lambda-ssa))
    (maybe-seal-block ssa-block lambda-ssa)))

(defun construct-ssa (lambda-ssa)
  (ssa-block-do-construction (first (lambda-ssa-blocks lambda-ssa))
			     lambda-ssa))


(defun ssa-reset-original-ir (lambda-ssa)
  (dolist (block (lambda-ssa-blocks lambda-ssa))
    (setf (ssa-block-ir block) nil))
  lambda-ssa)

(defun ssa-lambda-index-blocks (lambda-ssa)
  (let ((index-map (make-hash-table)))
    (dolist (block (lambda-ssa-blocks lambda-ssa))
      (setf (gethash (ssa-block-index block) index-map) block))
    (setf (lambda-ssa-blocks-index lambda-ssa) index-map)))

(defun set-block-virtuals (lambda-ssa)
  (dolist (block (lambda-ssa-blocks lambda-ssa))
    (setf (ssa-block-virtuals block)
	  (collect-block-virtuals block))))

(defun fill-blocks-ordering (lambda-ssa)
  (let ((start 0))
    (dolist (block (lambda-ssa-blocks lambda-ssa))
      (setf (ssa-block-order block) start)
      (incf start))))

(defun lambda-construct-ssa (lambda-node)
  (let* ((*ssa-block-counter* 0)
	 (*ir-index-counter* 0)
	 (*ssa-symbol-counter* 0)
	 (lambda-ssa (make-lambda-ssa))
	 (entry-block (make-new-ssa-block)))
    (ssa-add-block lambda-ssa entry-block)
    (emit lambda-ssa (make-lambda-entry) entry-block)
    (emit-lambda-arguments-ssa (lambda-node-arguments lambda-node ) lambda-ssa entry-block)
    (emit-ssa (lambda-node-body lambda-node) lambda-ssa t nil entry-block)
    (ssa-normalize-lambda-ssa lambda-ssa)
    (ssa-lambda-index-blocks lambda-ssa)
    (ssa-maybe-fix-blocks-connections lambda-ssa)
    (fill-blocks-predecessors lambda-ssa)
    (construct-ssa lambda-ssa)
    (ssa-reset-original-ir lambda-ssa)
    (set-block-virtuals lambda-ssa)
    (remove-redundant-phis lambda-ssa)
    (fill-blocks-ordering lambda-ssa)
    (compute-block-order lambda-ssa)
    lambda-ssa))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; block order

;;; FIXME
;;; Sometimes we can mark direct predecessor block as LOOP-END block
;;; remove LOOP-END from BLOCK in which LOOP-HEADER BLOCK is direct successor

(defun cbo-visit (block previous-block lambda-ssa visited active)
  (let* ((index (ssa-block-index block))
	 (b-visited (gethash index visited))
	 (b-active (gethash index active))
	 (successors (ssa-block-successors block lambda-ssa)))
    (unless b-visited
      (setf (gethash index visited) t))
    (if (> b-active 0)
	(progn
	  (unless (ssa-block-is-header block)
	    (setf (ssa-block-is-header block) (get-block-unique-header-number)))
	  (incf (ssa-block-branch-to-count block))
	  (setf (ssa-block-is-loop-end previous-block) t))
	(progn
	  (incf (gethash index active))
	  (dolist (sblock successors)
	    (cbo-visit sblock block lambda-ssa visited active))
	  (decf (gethash index active))))))

(defun compute-block-order (lambda-ssa)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((visited (make-hash-table))
	(active (make-hash-table))
	(*block-header-counter* 0))
    (dolist (block (lambda-ssa-blocks lambda-ssa))
      (setf (gethash (ssa-block-index block) active) 0))
    (cbo-visit (first (lambda-ssa-blocks lambda-ssa)) nil lambda-ssa visited active)
    ;; check
    (dolist (block (lambda-ssa-blocks lambda-ssa))
      (unless (gethash (ssa-block-index block) visited)
	(error "Not all blocks are visited")))
    active))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; interval building
(defparameter *interval-counter* 0)
(defstruct interval name (number (incf *interval-counter*)) ranges register stack child parent)
(defstruct range start end use-positions)
(defstruct use-pos index need-reg)
(defstruct (use-write-pos (:include use-pos)))
(defstruct (use-read-pos (:include use-pos)))

(defun interval-add-child (alloc i1 i2)
  (let ((i1-parent (interval-parent i1))
	(i1-ranges (interval-ranges i1))
	(i1-num (interval-number i1))
	(i2-num (interval-number i2)))
    (if i1-ranges
	(progn
	  (setf (interval-parent i2) i1-num)
	  (setf (interval-child i1) i2-num))
	(progn
	  (let ((parent-interval (alloc-get-handled-interval alloc i1-parent)))
	    (setf (interval-child parent-interval) i2-num)
	    (setf (interval-parent i2) i1-parent))))))

(defun intervals-same-storage-p (i1 i2)
  (and
   (eq (interval-name i1) (interval-name i2))
   (equalp (interval-register i1) (interval-register i2))
   (equalp (interval-stack i1) (interval-stack i2))))

(defun make-interval-storage (interval)
  (let ((register (interval-register interval))
	(stack (interval-stack interval)))
    (if register
	(make-reg-storage :register register)
	(make-stack-storage :offset stack))))

(defun interval-end (interval)
  (range-end (car (last (interval-ranges interval)))))

(defun interval-start (interval)
  (range-start (first (interval-ranges interval))))

(defun interval-live-at-index-p (interval index)
  (do* ((ranges (interval-ranges interval) (cdr ranges))
	(range (car ranges) (car ranges)))
       ((or (null range)
	    (> (range-start range)
	       index))
	nil)
    (when (and (>= index (range-start range))
	       (<= index (range-end range)))
      (return t))))

(defun ssa-place (place)
  (typecase place
    (virtual-place place)
    (phi-place place)
    (phi (phi-place place))))

(defun ssa-form-write-place (ssa-form)
  (etypecase ssa-form
    (ssa-form-rw
     (etypecase ssa-form
       (ssa-load (ssa-load-to ssa-form))
       (ssa-value nil)
       (ssa-if nil)))
    (t nil)))

(defun ssa-form-read-place (ssa-form)
  (etypecase ssa-form
    (ssa-form-rw
     (etypecase ssa-form
       (ssa-load (ssa-load-from ssa-form))
       (ssa-value (ssa-value-value ssa-form))
       (ssa-if (ssa-if-test ssa-form))))
    (t nil)))

(defun collect-block-virtuals (block)
  (let ((reads nil)
	(writes nil))
    (dolist (ir (ssa-block-ssa block))
      (let ((write (ssa-place (ssa-form-write-place ir)))
	    (read (ssa-place (ssa-form-read-place ir))))
	(when write
	  (pushnew write writes :test #'equalp))
	(when read
	  (pushnew read reads :test #'equalp))))
    (list reads writes)))

(defun is-block-loop-header (block lambda-ssa)
  ;; FIXME, recognize if block is loop header
  (dolist (b (lambda-ssa-blocks lambda-ssa))
    (when (= (ssa-block-index block)
	     (or (ssa-block-cond-jump b)
		 (ssa-block-uncond-jump b)
		 most-positive-fixnum))
      (return t))))

(defun ssa-block-successors-indexes (ssa-block)
  (remove-if #'null (list (ssa-block-succ ssa-block)
			  (ssa-block-cond-jump ssa-block)
			  (ssa-block-uncond-jump ssa-block))))

(defun ssa-block-successors (ssa-block lambda-ssa)
  (mapcar (lambda (index)
	    (ssa-find-block-by-index lambda-ssa index))
	  (ssa-block-successors-indexes ssa-block)))

(defun merge-all-blocks-live-in (successors-live-in)
  (let ((live nil))
    (dolist (li successors-live-in)
      (setf live (union live li :test #'equalp)))
    live))

(defun remove-from-live (live place)
  (remove place live :test #'equalp))

(defun live-add-phis-operands (live phis block)
  (let ((block-virtuals (ssa-block-virtuals block)))
    (dolist (phi phis)
      (when (phi-p phi)
	(dolist (pop (phi-operands phi))
	  (when (and (not (find pop live :test #'equalp))
		     ;; only add to live if operand is defined in this block
		     ;; paper live.add(phi.inputOf(b))
		     (or (find pop (first block-virtuals) :test #'equalp)
			 (find pop (second block-virtuals) :test #'equalp)))
	    (push pop live))))))
  live)

(defun make-intervals ()
  (make-hash-table))

(defun make-use-positions ()
  (make-hash-table))

(defun add-use-positions (use-positions virtual use-position)
  (let ((vname (named-place-name virtual)))
    (setf (gethash vname use-positions)
	  (cons use-position
		(gethash vname use-positions)))))

(defun get-use-positions (use-positions vname)
  (gethash vname use-positions))

(defun get-interval (intervals name)
  (gethash name intervals))

(defun add-interval (intervals interval)
  (setf (gethash (interval-name interval) intervals) interval))

(defun add-range (intervals name start end)
  (let ((interval (get-interval intervals name)))
    (unless interval
      (setf interval (make-interval :name name ))
      (add-interval intervals interval))
    (let ((active-range (first (interval-ranges interval))))
      (when (or (null active-range)
		(< start (range-start active-range))
		(> end (range-end active-range)))    
	(push (make-range :start start :end end)
	      (interval-ranges interval)))
      interval)))

(defun shorten-current-range (intervals place start)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let* ((name (named-place-name place))
	 (interval (get-interval intervals name)))
    (when interval
      (let ((range (first (interval-ranges interval))))
	(setf (range-start range) start)))))

(defun maybe-merge-ranges (ranges)
  (let (merged-ranges)
    (let* ((sorted (sort ranges #'< :key #'range-start))) 
      (dolist (range sorted)
	(let ((current-range (first merged-ranges)))
	  (if (null current-range)
	      (push range merged-ranges)
	      (if (> (range-start range)
		     (+ (range-end current-range) *instr-offset*))
		  (push range merged-ranges)
		  (when (> (range-end range)
			   (range-end current-range))
		    (setf (range-end current-range)
			  (range-end range))))))))
    (reverse merged-ranges)))

(defun range-split (range position)
  (if (= position (range-start range))
      (values nil range)
      (let ((old-range (make-range :start (range-start range) :end (- position *instr-offset*)))
	    (new-range (make-range :start position :end (range-end range))))
	(dolist (pos (range-use-positions range))
	  (if (<= (use-pos-index pos)
		  (range-end old-range))
	      (push pos (range-use-positions old-range))
	      (push pos (range-use-positions new-range))))
	(setf (range-use-positions old-range)
	      (reverse (range-use-positions old-range)))
	(setf (range-use-positions new-range)
	      (reverse (range-use-positions new-range)))
	(values old-range new-range))))

(defun interval-get-next-use (interval &optional (index 0))
  (prog1 0
    (dolist (range (interval-ranges interval))
      (unless (< (range-end range) index)
	(dolist (use-pos (range-use-positions range))
	  (let ((up-index (use-pos-index use-pos)))
	    (when (>= up-index index)
	      (return-from interval-get-next-use up-index))))))))

(defun split-interval (interval position)
  (let* ((range-position (position position (interval-ranges interval)
				   :test (lambda (v e)
					   (and (>= v (range-start e))
						(<= v (range-end e))))))
	 (interval-ranges (interval-ranges interval))
	 (rest-ranges (nthcdr range-position (interval-ranges interval)))
	 (split-range (first rest-ranges))
	 (interval-name (interval-name interval)))
    (multiple-value-bind (first-range second-range)
	(range-split split-range position)
      (if first-range
	  (setf (interval-ranges interval)
		(append
		 (subseq interval-ranges 0 range-position)
		 (list first-range)))
	  (setf (interval-ranges interval) nil)
	  ;; (setf interval nil)
	  )
      (values interval
	      (make-interval :name interval-name
			     :ranges (cons second-range
					   (cdr rest-ranges)))))))


(defun try-intervals-merge (intervals)
  (maphash (lambda (k interval)
	     (declare (ignore k))
	     (setf (interval-ranges interval)
		   (maybe-merge-ranges (interval-ranges interval))))
	   intervals)
  intervals)

(defun ranges-intersection (r1 r2)
  (block nil
    (tagbody
     loop-start
       (when (or (null r1)
		 (null r2))
	 (return  nil))
       (let ((first-range (first r2))
	     (first-c-range (first r1)))
	 (when (> (range-start first-range)
		  (range-end first-c-range))
	   (setf r1 (cdr r1))
	   (go loop-start))
	 (when (> (range-start first-c-range)
		  (range-end first-range))
	   (setf r2 (cdr r2))
	   (go loop-start))
	 (return (max (range-start first-range )
		      (range-start first-c-range)))))))

(defun intervals-first-intersection (i1 i2)
  (ranges-intersection (interval-ranges i1)
		       (interval-ranges i2)))

;;; FIXME, delete this, we don't need it
;; (defun intervals-next-intersection (current-interval interval current-position)
;;   (let* ((range-position (position current-position (interval-ranges current-interval) :key #'range-start))
;; 	 (current-ranges (nthcdr range-position (interval-ranges current-interval)))
;; 	 (ranges (interval-ranges interval)))
;;     (ranges-intersection current-ranges ranges)))

(defun interval-first-range-after-index (interval index)
  (dolist (range (interval-ranges interval))
    (when (> (range-start range) index)
      (return range))))

(defun fill-use-positions (interval pos)
  (let ((spos (sort pos #'< :key #'use-pos-index)))
    (dolist (range (interval-ranges interval))
      (do* ((sspos spos (cdr sspos))
	    (pos (car sspos) (car sspos)))
	   ((or
	     (null pos)
	     (when (< (use-pos-index pos)
		      (range-start range))
	       (error "Bad ranges/positions"))
	     (> (use-pos-index pos)
		(range-end range)))
	    (setf (range-use-positions range)
		  (reverse (range-use-positions range))))
	(push pos (range-use-positions range))
	(pop spos)))))

(defun add-intervals-use-positions (intervals use-positions)
  (let (itls)
    (maphash (lambda (name interval)
	       (fill-use-positions interval (get-use-positions use-positions name) )
	       (push interval itls))
	     intervals)
    (sort itls #'< :key #'interval-start)))

;;; FIXME, RANGE-END should be exclusive

(defun build-intervals (lambda-ssa)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((intervals (make-intervals))
	(use-positions (make-use-positions)))
    
    (dolist (block (reverse (lambda-ssa-blocks lambda-ssa)))
      (let* ((successor-blocks (ssa-block-successors block lambda-ssa))
	     (successors-live-in (mapcar #'ssa-block-live-in successor-blocks))
	     (live (merge-all-blocks-live-in successors-live-in)))

	(dolist (sb successor-blocks)
	  (let ((phis (ssa-block-all-phis sb)))
	    (setf live (live-add-phis-operands live phis block))))

	(when (ssa-block-first-instruction block)
	  (let ((start (ssa-form-index (ssa-block-first-instruction block)))
		(end (ssa-form-index (ssa-block-last-instruction block))))
	  
	    (dolist (place live)
	      (let ((place-name (named-place-name place)))
		(add-range intervals place-name start end)))

	    (dolist (instr (reverse (ssa-block-ssa block)))
	      (let ((instr-index (ssa-form-index instr))
		    (write (ssa-place (ssa-form-write-place instr)))
		    (read (ssa-place (ssa-form-read-place instr))))
		(when write
		  ;; FIXME, just one write to place that is never read
		  ;; we sure need to allocate register for this
		  (unless (shorten-current-range intervals write instr-index)
		    (add-range intervals (named-place-name write) instr-index instr-index))
		  (setf live (remove-from-live live write))
		  (add-use-positions use-positions write (make-use-write-pos :index instr-index)))
		;; FIXME, read is allways adding RANGE
		;; when we then have WRITE we only shorten last RANGE
		(when read
		  ;; maybe we have removed redundant PHI, int that case this is just some VIRTUAL-PLACE
		  (when (phi-place-p read)
		    (let* ((maybe-phi (get-phi-value-replacement read lambda-ssa))
			   (maybe-local-phi (ssa-block-get-maybe-phi block read))
			   (new-read (or maybe-phi maybe-local-phi)))
		      (when (and new-read
				 (not (phi-p new-read)))
			(setf read new-read))))
		
		  (add-range intervals (named-place-name read) start instr-index)
		  (add-use-positions use-positions read (make-use-read-pos :index instr-index))
		  (pushnew read live :test #'equalp))))

	    (dolist (phi (ssa-block-all-phis block))
	      (when (phi-p phi)
		(let ((phi-place (phi-place phi)))
		  (setf live (remove-from-live live phi-place)))))

	    (when (ssa-block-is-header block)
	      (let ((pindexes (ssa-block-predecessors block))
		    (this-block-index (ssa-block-index block)))
		(dolist (pindex pindexes)
		  (unless (= pindex (1- this-block-index))
		    (let ((pblock (ssa-find-block-by-index lambda-ssa pindex)))
		      ;; FIXME, do we care just for BACKWARD JUMP ??
		      (when (> pindex this-block-index)
			(dolist (lplace live)
			  (print (list 'lplace lplace))
			  (add-range intervals (named-place-name lplace) start (ssa-block-last-index pblock )))))))))
	    ))
	(setf (ssa-block-live-in block) live)))
    (try-intervals-merge intervals)
    (add-intervals-use-positions intervals use-positions)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alternative Intervals building/Linear Scan from "Linear scan register allocation for Java HotSpot Client Compiler"

(defun insert-block-between (before-block after-block)
  (let ((new-block (make-new-ssa-block))
	(before-index (ssa-block-index before-block))
	(after-index (ssa-block-index after-block)))
    (setf (ssa-block-succ new-block) before-index)
    (setf (ssa-block-predecessors before-block)
	  (substitute (ssa-block-index new-block) after-index
		      (ssa-block-predecessors before-block)))
    (cond ((= before-index (ssa-block-succ after-block))
	   (setf (ssa-block-succ after-block) (ssa-block-index new-block)))
	  ((= before-index (ssa-block-cond-jump after-block))
	   (setf (ssa-block-cond-jump after-block) (ssa-block-index new-block)))
	  ((= before-index (ssa-block-uncond-jump after-block))
	   (setf (ssa-block-uncond-jump after-block) (ssa-block-index new-block))))
    new-block))

(defun find-phi-operand-block (blocks operand)
  (dolist (b blocks)
    (when (or (find operand (first (ssa-block-virtuals b)) :test #'equalp)
	      (find operand (second (ssa-block-virtuals b)) :test #'equalp))
      (return b))))

#+nil(defun insert-moves-for-phi (lambda-ssa)
       (dolist (block lambda-ssa)
	 (let ((phis (ssa-block-phis block))
	       (predecessors (ssa-block-predecessors block)))
	   (when phis
	     (unless (> (length predecessors) 1)
	       (error "We should have more than 1 predecessor here !"))
	     (let ((pblocks (mapcar (lambda (i)
				      (ssa-find-block-by-index lambda-ssa i))
				    predecessors)))
	       (dolist (operand (phi-operands phi))
		 (let ((operand-block (find-phi-operand-block pblocks operand)))
		   (unless operand-block
		     (error "Can't find block for PHI operand"))
		   (when (> 1 (length (ssa-block-successors-indexes operand-block)))
		     (setf operand-block (insert-block-between block operand-block)))
		   )))))))

(defun compute-local-live-sets (lambda-ssa)
  (dolist (block (lambda-ssa-blocks lambda-ssa))
    (dolist (sform (ssa-block-ssa block))
      (let ((read (ssa-place (ssa-form-read-place sform )))
	    (write (ssa-place (ssa-form-write-place sform))))

	(when (and read
		   (not (find read (ssa-block-live-kill block) :test #'equalp)))
	  (pushnew read (ssa-block-live-gen block) :test #'equalp))
	(when write
	  (pushnew write (ssa-block-live-kill block) :test #'equalp)))))
  lambda-ssa)

(defun compute-global-live-sets (lambda-ssa)
  (let ((blocks (reverse (lambda-ssa-blocks lambda-ssa)))
	(changed nil))
    (tagbody
     start
       (setf changed nil)
       (dolist (block blocks)
	 (let ((live-out nil))
	   (dolist (sblock (ssa-block-successors block lambda-ssa))
	     (setf live-out (union live-out (ssa-block-live-in sblock) :test #'equalp)))
	   (setf (ssa-block-live-out block) live-out)
	   (let ((old-live-in (ssa-block-live-in block))
		 (live-in (union (set-difference (ssa-block-live-out block)
						 (ssa-block-live-kill block) :test #'equalp)
				 (ssa-block-live-gen block) :test #'equalp)))
	     (when (/= (length old-live-in)
		       (length live-in))
	       (setf changed t))
	     (setf (ssa-block-live-in block) live-in))))
       (when changed
	 (go start)))
    lambda-ssa))

;;; FIXME
#+nil(defun resolve-data-flow (lambda-ssa)
  (dolist (from (lambda-ssa-blocks lambda-ssa))
    (let ((successors (ssa-block-successors block lambda-ssa))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *stack-offset* -1)
(defun get-stack-index ()
  (incf *stack-offset*))

(defstruct alloc unhandled active inactive handled (per-name-handled (make-hash-table)) (block-intervals (make-hash-table)))

(defun alloc-add-unhandled (alloc interval)
  (push interval (alloc-unhandled alloc))
  (setf (alloc-unhandled alloc)
	(sort (alloc-unhandled alloc) #'< :key #'interval-start)))

(defun alloc-remove-active (alloc interval)
  (setf (alloc-active alloc)
	(remove (interval-number interval)
		(alloc-active alloc) :key #'interval-number)))

(defun alloc-add-active (alloc interval)
  (push interval (alloc-active alloc)))

(defun alloc-remove-inactive (alloc interval)
  (setf (alloc-inactive alloc)
	(remove (interval-number interval)
		(alloc-inactive alloc) :key #'interval-number)))

(defun alloc-add-handled (alloc interval)
  (setf (alloc-handled alloc)
	(acons (interval-number interval) interval
	       (alloc-handled alloc)))
  (let ((name (interval-name interval)))
    (push interval (gethash name (alloc-per-name-handled alloc))))
  (alloc-remove-active alloc interval)
  (alloc-remove-inactive alloc interval))

(defun alloc-get-handled-interval (alloc interval-num)
  (cdr (assoc interval-num (alloc-handled alloc))))

(defun alloc-add-inactive (alloc interval)
  (push interval (alloc-inactive alloc)))

(defun linear-scan (sorted-intervals)
  (let ((alloc (make-alloc :unhandled sorted-intervals))
	(*stack-offset* -1))
    (tagbody
     START
       (let ((current-interval (pop (alloc-unhandled alloc))))
	 (when current-interval
	   (progn

	     (let ((position (interval-start current-interval)))
	
	       (dolist (act-interval (alloc-active alloc))
		 
		 (cond ((< (interval-end act-interval) position)
			(alloc-remove-active alloc act-interval)
			(alloc-add-handled alloc act-interval))
		       ((not (interval-live-at-index-p act-interval position))
			(alloc-remove-active alloc act-interval)
			(alloc-add-inactive alloc act-interval))))

	       (dolist (ia-interval (alloc-inactive alloc))
		 
		 (cond ((< (interval-end ia-interval) position)
			(alloc-remove-inactive alloc ia-interval)
			(alloc-add-handled alloc ia-interval))
		       ((interval-live-at-index-p ia-interval position)
			(alloc-remove-inactive alloc ia-interval)
			(alloc-add-active alloc ia-interval))))

	       (let ((register (try-allocate-free-reg current-interval position alloc)))
		 (unless register
		   (allocate-blocked-reg current-interval alloc))))

	     (go start)))))
    (dolist (int (alloc-active alloc))
      (alloc-add-handled alloc int))

    (dolist (int (alloc-inactive alloc))
      (alloc-add-handled alloc int))
    alloc))


(defun collect-interval-childs (alloc interval)
  (let ((current-interval interval)
	(childs nil))
    (tagbody
     start
       (let ((child-num (interval-child current-interval)))
	 (when child-num
	   (let ((child-interval (alloc-get-handled-interval alloc child-num)))
	     (push child-interval childs)
	     (setf current-interval child-interval)
	     (go start)))))
    (when childs
      (cons interval (reverse childs)))))

(defun generate-split-intervals-moves (alloc)
  (let ((moves (make-hash-table)))
    (dolist (pair (alloc-handled alloc))
      (let ((interval (cdr pair)))
	(when (and (interval-child interval)
		   (not (interval-parent interval)))
	  (let ((intervals (collect-interval-childs alloc interval))
		(current-interval nil))
	    (dolist (intv intervals)
	      (when current-interval
		(let ((load (make-ssa-load :index (1+ (interval-end current-interval))
					   :from  (make-interval-storage current-interval)
					   :to (make-interval-storage intv))))
		  (push load (gethash (ssa-form-index load) moves))))
	      (setf current-interval intv))))))
    moves))

(defun make-positions ()
  (make-hash-table))

(defun add-position (positions position register)
  (when position
    (let ((pos (gethash register positions)))
      (when (or (null pos)
		(< position pos))
       (setf (gethash register positions) position)))))

(defun get-register-with-max-position (positions)
  (let ((rp (cons nil nil)))
    (maphash (lambda (k v)
	       (if (or (null (car rp))
 		       (> v (cdr rp)))
		   (setf  rp (cons k v))))
	     positions)
    rp))

(defun find-interval-that-use-reg (intervals register)
  (find register intervals :key #'interval-register))

(defun spill-interval (alloc interval)
  (setf (interval-stack interval) (get-stack-index))
  (setf (interval-register interval) nil)
  (alloc-add-handled alloc interval))

(defparameter *regs* (list :rax :rbx))

(defun try-allocate-free-reg (current-interval current-position alloc)
  (declare (ignore current-position))
  ;; (print (list 'try-allocate-free-reg current-interval))
  (let ((fup (make-positions)))
    (dolist (reg *regs*)
      (add-position fup most-positive-fixnum reg))
    (dolist (interval (alloc-active alloc))
      (add-position fup 0 (interval-register interval)))
    (dolist (interval (alloc-inactive alloc))
      (when (intervals-first-intersection current-interval interval)
	(add-position fup (interval-register interval)
		      (intervals-first-intersection current-interval interval))))
    (let* ((reg-pair (get-register-with-max-position fup))
	   (reg (car reg-pair))
	   (reg-pos (cdr reg-pair)))

      ;; (print (list 'reg 'pos reg reg-pos))
      (cond ((zerop reg-pos)
	     nil)
	    ((< (interval-end current-interval) reg-pos)
	     (setf (interval-register current-interval) reg)
	     (alloc-add-active alloc current-interval))
	    (t
	     (multiple-value-bind (original-interval new-interval)
		 (split-interval current-interval reg-pos)
	       ;; (print (list 'try-allocate-free-reg 'splitint-interval current-interval) )
	       ;; (print (list 'try-allocate-free-reg 'split-interval original-interval new-interval))
	       (interval-add-child alloc original-interval new-interval)
	       (setf (interval-register original-interval) reg)
	       (alloc-add-active alloc original-interval)
	       (alloc-add-unhandled alloc new-interval)))))))

;;; TODO
;;; Implement fixed intervals
;;; Currently we don't look if USE-POS needs register so we dont' split at those position
;;; FIXME, implement splitting at USE-POS that needs register (for VOP for example)
(defun allocate-blocked-reg (current-interval alloc)
  (declare (optimize (speed 0) (debug 3) (safety 3)))
  ;; (print (list 'allocate-blocked-reg current-interval))

  (let ((positions (make-positions))
	(current-index (interval-start current-interval))
	(current-first-usage (interval-get-next-use current-interval)))

    (dolist (reg *regs*)
      (add-position positions most-positive-fixnum reg))
    
    (dolist (interval (alloc-active alloc))
      (add-position positions (interval-get-next-use interval (1+ current-index))
		    (interval-register interval)))
    
    (dolist (interval (alloc-inactive alloc))
      (if (intervals-first-intersection current-interval interval)
	  (add-position positions (interval-get-next-use interval (1+ current-index))
			(interval-register interval))))

    (let* ((reg-pair (get-register-with-max-position positions))
	   (reg (car reg-pair))
	   (reg-pos (cdr reg-pair)))
      ;; (print (list 'reg 'pos reg reg-pos))

      (cond ((> current-first-usage reg-pos)
	     ;; (multiple-value-bind (original-interval new-interval)
	     ;; 	 ;; FIXME, currently we don't split at first USE-POINT that need register
	     ;; 	 ;; we just spill whole current interval
	     ;; 	 ;; when we implement VOP with fixed register change this to split
	     ;; 	 (split-interval current-interval current-first-usage)

	     ;;   (interval-add-child alloc original-interval new-interval)
	     ;;   ;; (print (list 'allocate-blocked-reg 'spliting-interval 1 current-interval))
	     ;;   ;; (print (list 'allocate-blocked-reg 'split-interval 1 original-interval new-interval))
	     ;;   (spill-interval alloc original-interval)
	     ;;   (alloc-add-unhandled alloc new-interval))
	     (spill-interval alloc current-interval))
	    (t
	     (let* ((ainterval (find-interval-that-use-reg (alloc-active alloc) reg)))
	       (multiple-value-bind (old-interval new-interval)
		   ;; spliting at current-index can yield RANGES NIL for old-interval
		   (split-interval ainterval current-index)
		 (interval-add-child alloc old-interval new-interval)
		 (spill-interval alloc new-interval)
		 (when (interval-ranges old-interval)
		   (alloc-add-handled alloc old-interval))
		 ;; FIXME, if there is USE-POSITION that need register we need to split it again
		 ;; look at function comment
		 (setf (interval-register current-interval) reg)
		 (alloc-add-active alloc current-interval)
		 ;; (print (list 'allocate-blocked-reg 'spliting-interval 2 ainterval))
		 ;; (print (list 'allocate-blocked-reg 'split-interval 2 old-interval new-interval))
		 (dolist (inactive-interval (find-interval-that-use-reg (alloc-inactive alloc) reg))
		   (multiple-value-bind (old-interval new-interval)
		       (split-interval inactive-interval
				       (range-start
					(interval-first-range-after-index inactive-interval current-index)))
		     (interval-add-child alloc old-interval new-interval)
		     (spill-interval alloc new-interval)
		     (when (interval-ranges old-interval)
		       (alloc-add-handled alloc old-interval)))))))))))

(defun make-block-used-intervals (lambda-ssa alloc)
  (let ((intervals (sort (mapcar #'cdr (alloc-handled alloc)) #'< :key #'interval-start))
	(block-intervals (make-hash-table)))
    (dolist (block (lambda-ssa-blocks lambda-ssa))
      (let ((block-start (ssa-block-first-index block))
	    (block-end (ssa-block-last-index block)))
	(dolist (interval intervals)
	  (let* ( (istart (interval-start interval))
		  (iend (interval-end interval)))
	    (if (or (and (>= istart block-start)
			 (<= istart block-end))
		    (and (>= iend block-start)
			 (<= iend block-end)))
		(push interval (gethash (ssa-block-index block) block-intervals))
		(when (> istart block-start)
		  (return)))))))
    (setf (alloc-block-intervals alloc) block-intervals)))

(defun get-first-or-last-intervals-in-block (block-index alloc what)
  (let ((block-intervals (gethash block-index (alloc-block-intervals alloc)))
	(fih (make-hash-table))
	(compare-fun (if (eq :first what) #'< #'>)))
    (dolist (interval block-intervals)
      (let ((existing-interval (gethash (interval-name interval) fih)))
	(if existing-interval
	    (when (funcall compare-fun (interval-start interval) (interval-start existing-interval))
	      (setf (gethash (interval-name interval) fih) interval))
	    (setf (gethash (interval-name interval) fih) interval))))
    fih))

(defun hash-block-phis (block)
  (let ((h (make-hash-table)))
    (dolist (phi (ssa-block-all-phis block))
      (setf (gethash (named-place-name (phi-place phi)) h) phi))
    h))

;;; FIXME
;;; when inserting resolve moves check to see if last instruction is JUMP
;;; if it is MOV's need to be inserted before JUMP

#+nil(defun resolve-phi-move (phi block lambda-ssa alloc)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (print (list 'resolve-phi-nove phi))
  (let ((predecessors (ssa-block-predecessors block)))
    ))

#+nil(defun resolve-maybe-split-interval-move (interval)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  )

(defun resolve-data-flow (lambda-ssa alloc)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (make-block-used-intervals lambda-ssa alloc)
  (dolist (pblock (lambda-ssa-blocks lambda-ssa))
    (let ((successors (ssa-block-successors pblock lambda-ssa))
	  (predecessor-block-last-intervals (get-first-or-last-intervals-in-block (ssa-block-index pblock) alloc :last)))
      (dolist (sblock successors)
	(let ((start-intervals (get-first-or-last-intervals-in-block (ssa-block-index sblock) alloc :first))
	      (sblock-phis (hash-block-phis sblock)))
	  (dolist (int (hash-values start-intervals))
	    (let ((maybe-phi (gethash (interval-name int) sblock-phis))
		  (interval-name (interval-name int)))
	      (if maybe-phi
		  ;; (resolve-phi-move maybe-phi sblock lambda-ssa alloc)
		  'FIXME
		  (when (or (interval-parent int)
			    (interval-child int))
		    (let ((pred-last-interval (gethash interval-name predecessor-block-last-intervals)))
		      ;; when interval is direct parent or child then move is already inserted when splitting
		      (when (and (/= (interval-number int) (interval-number pred-last-interval))
				 (not (or (= (interval-parent int) (interval-number pred-last-interval))
					  (= (interval-parent pred-last-interval) (interval-number int)))))
			;; insert MOVE
			(print (list pred-last-interval int)))))))))))))

;;; Test case that currently doesn't work
#+nil(lambda-construct-ssa (create-node (clcomp-macroexpand '(lambda (a)
						     (let ((c 0))
						       (tagbody 
							bar
							  (setf c (+ c 1))
							  (when a (go bar)))
						       c)))))

#+nil(lambda-construct-ssa (create-node (clcomp-macroexpand '(lambda (a)
								 (tagbody
								    (when 1 (go third))
								  second
								    (print 1)
								  third
								    (when 2 (go second)))
							 a))))
