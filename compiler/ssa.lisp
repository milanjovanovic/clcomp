(in-package :clcomp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (debug 3))))

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
(defstruct (var-place (:include named-place)))

(defstruct (fixup (:include named-place)))
(defstruct (local-component-fixup (:include fixup)))
(defstruct (compile-function-fixup (:include fixup)) function)
(defstruct (load-time-eval-fixup (:include fixup)))
(defstruct (compile-time-constant-fixup (:include fixup)) form)

(defstruct slabels alist)
(defstruct ssa-env labels)
(defstruct lambda-ssa blocks blocks-index (env (make-ssa-env)) fixups sub-lambdas)

(defstruct ssa-block index ir ssa succ cond-jump uncond-jump predecessors label defined
  live-in virtuals live-gen live-kill live-out)

(defstruct ssa-form index)
(defstruct (lambda-entry (:include ssa-form)))
(defstruct (arg-check (:include ssa-form)) arg-count)
(defstruct (ssa-form-rw (:include ssa-form)))

(defstruct (ssa-load (:include ssa-form-rw)) to from)
(defstruct (ssa-go (:include ssa-form)) label)
(defstruct (ssa-label (:include ssa-form)) label)
;;; do we need SSA-VALUE ??
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


(defun lambda-add-fixup (fixup lambda-ssa)
  (push fixup (lambda-ssa-fixups lambda-ssa)))

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
       (add-sub-lambda lambda-ssa (ssa-parse-lambda (load-time-value-node-node node)) fixup))
      (lambda-node
       (add-sub-lambda lambda-ssa (ssa-parse-lambda node) fixup)))
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

(defstruct phi operands (place (generate-virtual-place "PHI-")))


(defun phi-add-operand (phi operand)
  (typecase operand
    (phi (push operand (phi-operands phi)))
    (t (push operand (phi-operands phi)))))

(defun get-block-def (block symbol)
  (cdr (assoc symbol (ssa-block-defined block))))

(defun set-block-def (block var value)
  (let ((cons (assoc var (ssa-block-defined block))))
    (if cons
	(setf (cdr cons) value)
	(setf (ssa-block-defined block)
	      (acons var value (ssa-block-defined block))))
    value))

(defun phi-has-operand (phi phi-operand)
  (dolist (op (phi-operands phi))
    (when (eq op phi-operand)
      (return-from phi-has-operand t))))

;; FIXME, not-phi removing phi that we want
(defun get-all-phis-acons (phi lambda-ssa)
  (let (phis)
    (dolist (block (lambda-ssa-blocks lambda-ssa))
      (dolist (acons (ssa-block-defined block))
	(when (and (phi-p (cdr acons))
		   (not (eq phi (cdr acons)))
		   (phi-has-operand (cdr acons) phi))
	  (pushnew acons phis :key #'cdr))))
    phis))

(defun replace-phi-operands (phi old-phi new-value)
  (let (new-operands)
    (dolist (operand (phi-operands phi))
      (if (eq operand old-phi)
	  (push new-value new-operands)
	  (push operand new-operands)))
    (setf (phi-operands phi) new-operands)))

(defun replace-phi (phi same lambda-ssa)
  (dolist (block (lambda-ssa-blocks lambda-ssa))
    (dolist (acons (ssa-block-defined block))
      (let ((p (cdr acons)))
	(when (phi-p p)
	  (if (eq p phi)
	      (setf (cdr acons) same)
	      (replace-phi-operands p phi same)))))))

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

(defun try-remove-trivial-phi (phi lambda-ssa)
  (let ((same nil))
    (dolist (operand (phi-operands phi))
      (cond ((or (eq operand same)
		 (eq operand phi)))
	    ((not (null same))
	     (return-from try-remove-trivial-phi phi))
	    (t (setf same operand))))
    (replace-phi phi same lambda-ssa)
    (let ((uses (get-all-phis-acons phi lambda-ssa)))
      (dolist (phi-cons uses)
	(let ((p (cdr phi-cons)))
	  (when (phi-p p)
	    (try-remove-trivial-phi p lambda-ssa)))))
    same))

(defun phi-add-operands (place phi predecessors lambda-ssa)
  (dolist (pblock predecessors)
    (phi-add-operand phi (ssa-read-variable place (ssa-find-block-by-index lambda-ssa pblock) lambda-ssa)))
  (try-remove-trivial-phi phi lambda-ssa))

(defun read-variable-recursive (place block lambda-ssa)
  (let ((predecessors (ssa-block-predecessors block)))
    (when predecessors
      (if (= (length predecessors) 1)
	  (set-block-def block (named-place-name place)
			 (ssa-read-variable place (ssa-find-block-by-index lambda-ssa (first predecessors)) lambda-ssa))
	  (let* ((phi (make-phi)))
	    (set-block-def block (named-place-name place) phi)
	    (set-block-def block (named-place-name place)
			   (phi-add-operands place phi predecessors lambda-ssa)))))))

(defun ssa-read-variable (place block lambda-ssa)
  (or (get-block-def block (named-place-name place)) 
      (read-variable-recursive place block lambda-ssa)))

(defun transform-write (place block lambda-ssa)
  (typecase place
    (named-place
     (ssa-write-variable place block lambda-ssa))
    (t place)))

(defun transform-read  (place block lambda-ssa)
  (typecase place
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
      (ssa-transform-block-ir b lambda-ssa)))
  lambda-ssa)

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

(defun ssa-parse-lambda (lambda-node)
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
    (do-value-numbering lambda-ssa)
    (ssa-reset-original-ir lambda-ssa)
    (set-block-virtuals lambda-ssa)
    lambda-ssa))

;;; interval building
(defparameter *interval-counter* 0)
(defstruct interval name (number (incf *interval-counter*)) ranges register stack childs)
(defstruct range start end use-positions)
(defstruct use-pos index need-reg)
(defstruct (use-write-pos (:include use-pos)))
(defstruct (use-read-pos (:include use-pos)))

(defun intervals-same-storage-p (i1 i2)
  (and
   (eq (interval-name i1) (interval-name i2))
   (equalp (interval-register i1) (interval-register i2))
   (equalp (interval-stack i1) (interval-stack i2))))

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


(defun ssa-block-successors (ssa-block lambda-ssa)
  (mapcar (lambda (index)
	    (ssa-find-block-by-index lambda-ssa index))
	  (remove-if #'null (list (ssa-block-cond-jump ssa-block)
				  (ssa-block-uncond-jump ssa-block)
				  (ssa-block-succ ssa-block)))))

(defun ssa-block-phis (sblock)
  (let ((phis nil))
    (dolist (pair (ssa-block-defined sblock))
      (when (phi-p (cdr pair))
	(push (cdr pair) phis )))
    phis))

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
      (dolist (pop (phi-operands phi))
	(when (and (not (find pop live :test #'equalp))
		   (or (find pop (first block-virtuals) :test #'equalp)
		       (find pop (second block-virtuals) :test #'equalp)))
	  (push pop live)))))
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
  (dolist (range (interval-ranges interval))
    (unless (< (range-end range) index)
      (dolist (use-pos (range-use-positions range))
	(let ((up-index (use-pos-index use-pos)))
	  (when (>= up-index index)
	    (return-from interval-get-next-use up-index)))))))

(defun split-interval (interval position)
  (let* ((range-position (position position (interval-ranges interval)
				   :test (lambda (v e)
					   (and (>= v (range-start e))
						(<= v (range-end e))))))
	 (interval-ranges (interval-ranges interval))
	 (rest-ranges (nthcdr range-position (interval-ranges interval)))
	 (split-range (first rest-ranges)))
    (multiple-value-bind (first-range second-range)
	(range-split split-range position)
      (if first-range
	  (setf (interval-ranges interval)
		(append
		 (subseq interval-ranges 0 range-position)
		 (list first-range)))
	  (setf (interval-ranges interval) nil))
      (values interval
	      (make-interval :name (interval-name interval)
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
	  (let ((phis (ssa-block-phis sb)))
	    (setf live (live-add-phis-operands live phis block))))

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
	      ;;; when we then have WRITE we only shorten last RANGE
	      (when read
		(add-range intervals (named-place-name read) start instr-index)
		(add-use-positions use-positions read (make-use-read-pos :index instr-index))
		(pushnew read live :test #'equalp))))

	  (dolist (phi (ssa-block-phis block))
	    (let ((phi-place (phi-place phi)))
	      (setf live (remove-from-live live phi-place))))

	  ;; (when (is-block-loop-header block lambda-ssa)
	  ;;   ;; FIXME, what when we have both cond and uncond JUMP
	  ;;   ;; FIXME, check if this is right
	  ;;   (let ((block-end (ssa-find-block-by-index lambda-ssa (or (ssa-block-cond-jump block)
	  ;; 							     (ssa-block-uncond-jump block)))))
	  ;;     (dolist (lplace live)
	  ;; 	(add-range intervals (named-place-name lplace) start
	  ;; 		   (ssa-form-index (ssa-block-last-instruction block-end))))))
	  
	  (setf (ssa-block-live-in block) live))))
    (try-intervals-merge intervals)
    (add-intervals-use-positions intervals use-positions)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alternative Intervals building/Linear Scan from "Linear scan register allocation for Java HotSpot Client Compiler"

;;; FIXME
;;; we sometimes need to create block as in IF form
(defun insert-moves-for-phi (lambda-ssa)
  (dolist (block lambda-ssa)
    (let ((phis (ssa-block-phis block)))
      (when phis
	(dolist (phi phis)
	  ())))))

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

(defstruct alloc unhandled active inactive handled)

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
  (push interval (alloc-handled alloc))
  (alloc-remove-active alloc interval)
  (alloc-remove-inactive alloc interval))

(defun alloc-add-inactive (alloc interval)
  (push interval (alloc-inactive alloc)))


(defun linear-scan (sorted-intervals)
  (let ((alloc (make-alloc :unhandled sorted-intervals))
	(allocated-intervals nil)
	(*stack-offset* -1))
    (tagbody
     START
       (let ((current-interval (first (alloc-unhandled alloc))))
	 (when current-interval
	   (progn

	     (push (pop (alloc-unhandled alloc)) allocated-intervals)
	       
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

    (alloc-handled alloc)))

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
  (print (list 'try-allocate-free-reg current-interval))
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

      (print (list 'reg 'pos reg reg-pos))
      (cond ((zerop reg-pos)
	     nil)
	    ((< (interval-end current-interval) reg-pos)
	     (setf (interval-register current-interval) reg)
	     (alloc-add-active alloc current-interval))
	    (t
	     (multiple-value-bind (original-interval new-interval)
		 (split-interval current-interval reg-pos)
	       (print (list 'try-allocate-free-reg 'splitint-interval current-interval) )
	       (print (list 'try-allocate-free-reg 'split-interval original-interval new-interval))
	       (push new-interval (interval-childs original-interval))
	       (setf (interval-register original-interval) reg)
	       (alloc-add-active alloc original-interval)
	       (alloc-add-unhandled alloc new-interval)))))))

;;; TODO
;;; Implement fixed intervals
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
		    (interval-register interval) ))
    
    (dolist (interval (alloc-inactive alloc))
      (if (intervals-first-intersection current-interval interval)
	  (add-position positions (interval-get-next-use interval (1+ current-index))
			(interval-register interval))))

    (let* ((reg-pair (get-register-with-max-position positions))
	   (reg (car reg-pair))
	   (reg-pos (cdr reg-pair)))

      ;; (print (list 'reg 'pos reg reg-pos))
 
      (cond ((>= current-first-usage reg-pos)
	     (multiple-value-bind (original-interval new-interval)
		 (split-interval current-interval current-first-usage)
	       (push new-interval (interval-childs original-interval))
	       ;; (print (list 'allocate-blocked-reg 'spliting-interval 1 current-interval))
	       ;; (print (list 'allocate-blocked-reg 'split-interval 1 original-interval new-interval))
	       (spill-interval alloc original-interval)
	       (alloc-add-unhandled alloc new-interval)))
	    (t
	     (let* ((ainterval (find-interval-that-use-reg (alloc-active alloc) reg)))
	       (multiple-value-bind (old-interval new-interval)
		   (split-interval ainterval current-index)
		 (push new-interval (interval-childs old-interval))
		 ;; (print (list 'allocate-blocked-reg 'spliting-interval 2 ainterval))
		 ;; (print (list 'allocate-blocked-reg 'split-interval 2 old-interval new-interval))
		 (alloc-add-handled alloc old-interval)
		 (setf (interval-register current-interval) reg)
		 (spill-interval alloc new-interval)
		 (alloc-add-active alloc current-interval)
		 (dolist (inactive-interval (find-interval-that-use-reg (alloc-inactive alloc) reg))
		   (multiple-value-bind (old-interval new-interval)
		       (split-interval inactive-interval
				       (range-start
					(interval-first-range-after-index inactive-interval current-index)))
		     (alloc-add-handled alloc old-interval)
		     (spill-interval alloc new-interval))))))))))


#+nil(defun intervals-that-start-at (intervals index)
  )


#+nil(defun resolve (lambda-ssa intervals)
  (dolist (cblock (lambda-ssa-blocks lambda-ssa))
    (let ((successors (ssa-block-successors cblock lambda-ssa)))
      (dolist (sblock successors)
	(let ((is (intervals-that-start-at intervals (ssa-block-first-index sblock))))
	  )))))
