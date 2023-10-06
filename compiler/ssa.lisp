(defpackage #:clcomp.ssa
  (:use :cl))

(in-package #:clcomp.ssa)

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
(defstruct (phi-place
	    (:print-function (lambda (struct stream depth)
			       (declare (ignore depth))
			       (format stream "#S(PHI-PLACE :NAME ~A :REDUCED ~A"
				       (named-place-name struct)
				       (funcall (phi-place-reduced struct)))))
	    (:include named-place)) reduced)
(defstruct (var-place (:include named-place)))

(defstruct (mvb-place (:include place)) var-places)

(defstruct (fixup (:include named-place)))
(defstruct (local-component-fixup (:include fixup)))
(defstruct (compile-function-fixup (:include fixup)) function)
(defstruct (load-time-eval-fixup (:include fixup)))
(defstruct (compile-time-constant-fixup (:include fixup)) form)
(defstruct slabels alist)
(defstruct ssa-env labels)
(defstruct lambda-ssa blocks (blocks-index (make-hash-table)) (block-order-index (make-hash-table))
	   (env (make-ssa-env)) fixups sub-lambdas
	   (phi-connections (make-hash-table)) loop-header-blocks loop-end-blocks
	   (redundant-phis (make-hash-table :test #'equalp)))

(defstruct ssa-block index order ir ir-last-cons ssa succ cond-jump uncond-jump predecessors is-loop-end is-header (branch-to-count 0)
  sealed processed label defined phis live-in virtuals live-gen live-kill live-out)

(defstruct ssa-form index)
(defstruct (lambda-entry (:include ssa-form)))
(defstruct (arg-check (:include ssa-form)) arg-count)
(defstruct (ssa-form-rw (:include ssa-form)))

;;;FIXME, find better name
(defstruct (ssa-return-values (:include ssa-form)) values-count)

(defstruct (ssa-load (:include ssa-form-rw)) to from)

(defstruct (ssa-go (:include ssa-form)) label)
(defstruct (ssa-label (:include ssa-form)) label)
;;; Do we need SSA-VALUE ??
(defstruct (ssa-value (:include ssa-form-rw)) value)

(defstruct (ssa-base-return (:include ssa-form)))
(defstruct (ssa-return (:include ssa-base-return)))
(defstruct (ssa-multiple-return (:include ssa-base-return)) count)

;;; FIXME, maybe we can split VOP to siple LOAD and WRITE IR instructions
(defstruct (ssa-vop (:include ssa-form-rw)) return-values args)

;;; FIXME, still not sure when to use which one
(defstruct (ssa-fun-call (:include ssa-form)) fun)
(defstruct (ssa-unknown-values-fun-call (:include ssa-fun-call)))
(defstruct (ssa-known-values-fun-call (:include ssa-fun-call)) min-values)

(defstruct (ssa-if (:include ssa-form-rw)) test true-block true-block-label false-block-label)

(defstruct (ssa-mvb-bind (:include ssa-form)) places)


(defparameter *break-block* -1)


;;; we can have more then one level of reduced value
;;; try to get reduced value of already reduced one until we get NIL
(defun get-phi-place-reduced-value (place &optional last)
  (if (and (phi-place-p place)
	   (phi-place-reduced place))
      (let ((red (funcall (phi-place-reduced place))))
	(if red
	    (get-phi-place-reduced-value red red)
	    last))
      last))

;;; because of REDUCED in PHI-PLACE we need custom NAMED-PLACE-NAME function
(defun get-place-name (place)
  (named-place-name (get-phi-place-reduced-value place place)))

(defun get-maybe-reduced-place (place)
  (get-phi-place-reduced-value place place))

(defun place-is-phi (place)
  (and (typep place 'phi-place)
       (not (funcall (phi-place-reduced place)))))


(defparameter *testb* nil)

(defparameter *instr-offset* 2)

(defparameter *optimize-redundant-phis* t)
(defparameter *optimize-redundant-blocks* t)

(defparameter *error-on-ir-touch* nil)
(defparameter *error-on-ssa-touch* nil)

(defun error-if-touch-ir ()
  (when *error-on-ir-touch*
    (error "Too late to touch IR")))

(defun error-if-touch-ssa ()
  (when *error-on-ssa-touch*
    (error "Too early to touch SSA")))

(defparameter *debug-stream* t)
(defun print-debug (&rest s)
  (format  *debug-stream* (apply #'concatenate 'string (mapcar #'write-to-string s)))
  (princ #\Newline *debug-stream*))


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

(defun generate-label-for-string (str)
  (prog1
      (make-symbol (concatenate 'string str "-" (write-to-string *ssa-symbol-counter*)))
    (incf *ssa-symbol-counter*)))

(defun generate-label-symbol ()
  (prog1
      (make-symbol (concatenate 'string "REPLACE-LABEL-" (write-to-string *ssa-symbol-counter*)))
    (incf *ssa-symbol-counter*)))

(defparameter *ssa-block-counter* 0)
(defun make-new-ssa-block (lambda-ssa)
  (let ((block (make-ssa-block :index *ssa-block-counter*)))
    (setf (gethash *ssa-block-counter* (lambda-ssa-blocks-index lambda-ssa)) block)
    (incf *ssa-block-counter*)
    block))

(defun destroy-ssa-block (block lambda-ssa)
  (remhash (ssa-block-index block) (lambda-ssa-blocks-index lambda-ssa))
  nil)

(defparameter *fixup-symbol-counter* 0)
(defun generate-fixup-symbol ()
  (prog1
      (make-symbol (concatenate 'string "FIXUP-" (write-to-string *fixup-symbol-counter*)))
    (incf *fixup-symbol-counter*)))

(defparameter *phi-place-symbol-counter* 0)
(defun generate-phi-place (lambda-ssa)
  (let ((symbol (make-symbol (concatenate 'string "PHI-PLACE-" (write-to-string *phi-place-symbol-counter*)))))
    (let ((place (make-phi-place :name symbol :reduced nil)))
      (setf (phi-place-reduced place)
	    (lambda ()
	      (gethash place (lambda-ssa-redundant-phis lambda-ssa))))
      (incf *phi-place-symbol-counter*)
      place)))

(defun generate-return-places (n)
  (let (rets)
    (dotimes (i n)
      (push (make-return-value-place :index i) rets))
    (reverse rets)))

(defun mvb-bind-to-fun-call (mvb-place block)
  (let ((index 0))
    (dolist (place (mvb-place-var-places mvb-place))
      (emit-ir (make-ssa-load :to place :from (clcomp::make-constant-nil-node)) block)
      (incf index))))

(defun move-to-place (to from block)
  (typecase to
    (mvb-place
     (typecase from
       (mvb-place
	;; throw error for now, in this case (if this can actually apear - do nothing)
	(error "mvb-place to mvb-place"))
       (otherwise
	(emit-ir (make-ssa-load :to (first (mvb-place-var-places to)) :from from) block))))
    (otherwise
     (typecase from
       (mvb-place (error "mvb-place to other place, should now happen ?"))
       (otherwise (emit-ir (make-ssa-load :to to :from from) block))))))

(defun lambda-add-fixup (fixup lambda-ssa)
  (push fixup (lambda-ssa-fixups lambda-ssa)))

(defun lambda-ssa-find-header-index (lambda-ssa end-block-index)
  (cdr (assoc end-block-index (lambda-ssa-loop-end-blocks lambda-ssa))))

;;; FIXME, all end blocks are in one plist
#+nil
(defun lambda-ssa-find-end-blocks (lambda-ssa header-block-index)
  (let ((indexes nil))
    (dolist (pair (lambda-ssa-loop-header-blocks lambda-ssa))
      (let ((header (car pair))
	    (end (cdr pair)))
	(when (= header-block-index header)
	  (push end indexes))))
    indexes))

(defun lambda-ssa-is-last-block (lambda-ssa block)
  (= (ssa-block-index block)
     (ssa-block-index (car (last (lambda-ssa-blocks lambda-ssa))))))

(defun lambda-ssa-find-end-blocks (lambda-ssa header-block-index)
  (getf (lambda-ssa-loop-header-blocks lambda-ssa) header-block-index))

(defun lambda-ssa-add-loop-end-block (header-index end-index lambda-ssa)
  (let ((ends (assoc header-index (lambda-ssa-loop-header-blocks lambda-ssa))))
    (if
     ends
     (setf (cdr ends) (cons end-index (cdr ends)))
     (push (cons header-index (list end-index))
	   (lambda-ssa-loop-header-blocks lambda-ssa)))))

(defun lambda-ssa-get-block-by-order (lambda-ssa order)
  (gethash order (lambda-ssa-block-order-index lambda-ssa)))

(defun lambda-ssa-get-previous-order-block (lambda-ssa sblock)
  (do ((order (ssa-block-order sblock) (1- order)))
      ((= order -1) nil)
    (let ((b (lambda-ssa-get-block-by-order lambda-ssa order)))
      (when b
	(return-from lambda-ssa-get-previous-order-block b)))))

(defun lambda-ssa-get-next-order-block (lambda-ssa sblock)
  (let ((maxord (apply #'max (clcomp::hash-keys (lambda-ssa-block-order-index lambda-ssa)))))
    (do ((order (+ 1 (ssa-block-order sblock)) (1+ order)))
	((> order maxord) nil)
      (let ((b (lambda-ssa-get-block-by-order lambda-ssa order)))
	(when b
	  (return-from lambda-ssa-get-next-order-block b))))
    nil))

(defun lambda-ssa-find-greatest-end-block (lambda-ssa header-block-index)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let* ((indexes (lambda-ssa-find-end-blocks lambda-ssa header-block-index))
	 (blocks (mapcar (lambda (i)
			   (ssa-find-block-by-index lambda-ssa i)) indexes)))
    (ssa-block-index (first (sort blocks (lambda (b1 b2)
					   (>= (ssa-block-order b1) (ssa-block-order b2))))))))

(defun ssa-block-is-next-block (b1 b2 lambda-ssa)
  (do* ((blocks (lambda-ssa-blocks lambda-ssa) (cdr blocks))
	(b (car blocks) (car blocks)))
       ((= (ssa-block-index b1) (ssa-block-index b))
	(let ((next-block (cadr blocks)))
	  (and next-block
	       (= (ssa-block-index next-block)
		  (ssa-block-index b2)))))))

(defun ssa-block-add-phi (ssa-block phi)
  (setf (ssa-block-phis ssa-block) (acons (named-place-name (phi-place phi)) phi
					  (ssa-block-phis ssa-block))))

(defun ssa-block-replace-phi (ssa-block phi-place new-value)
  (let ((cons (assoc (named-place-name phi-place) (ssa-block-phis ssa-block))))
    (setf (cdr cons) new-value)))

(defun ssa-block-maybe-replace-phi (ssa-block phi-place new-value)
  (let ((cons (assoc (named-place-name phi-place) (ssa-block-phis ssa-block))))
    (when cons
      (setf (cdr cons) new-value))))

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

;;; FIXME, when addiing replacement we need to create new operand to PHI connection
(defun add-phi-value-replacement (phi value lambda-ssa)
  (let ((phi-place (phi-place phi)))
    (setf (gethash phi-place (lambda-ssa-redundant-phis lambda-ssa)) value)
    (add-phi-connections phi value lambda-ssa)))

(defun get-phi-value-replacement (phi-place lambda-ssa)
  (gethash phi-place (lambda-ssa-redundant-phis lambda-ssa)))

(defun maybe-get-simplified-phi-value (place block lambda-ssa)
  (declare (ignore block lambda-ssa))
  (if (phi-place-p place)
      (let ((reduced (get-phi-place-reduced-value place)))
	(or reduced place))
      place))

(defun ssa-block-all-phis (ssa-block)
  (mapcar #'cdr (ssa-block-phis ssa-block)))

(defun ssa-add-block (lambda-ssa block)
  (if (lambda-ssa-blocks lambda-ssa)
      (push block (cdr (last (lambda-ssa-blocks lambda-ssa))))
      (push block (lambda-ssa-blocks lambda-ssa))))

(defun ssa-connect-blocks (b1 b2)
  (if (ssa-block-succ b1)
      (error "Block already have successor !")
      (progn
	(setf (ssa-block-succ b1) (ssa-block-index b2))
	(push (ssa-block-index b1) (ssa-block-predecessors b2)))))

(defun ssa-maybe-connect-blocks (b1 b2)
  (if (ssa-block-uncond-jump b1)
      (error "Block already have UNCOND-JUMP")
      (ssa-connect-blocks b1 b2)))

(defun insert-block-conditional-jump (block to-block)
  (setf (ssa-block-cond-jump block) (ssa-block-index to-block))
  (push (ssa-block-index block) (ssa-block-predecessors to-block)))

(defun insert-block-unconditional-jump (block to-block)
  (if (ssa-block-uncond-jump block)
      (error "Block already have UNCOND-JUMP")
      (progn
	(setf (ssa-block-uncond-jump block) (ssa-block-index to-block))
	(push (ssa-block-index block) (ssa-block-predecessors to-block))
	(when (ssa-go-p (ssa-block-ir-last-instr block))
	  (error "Block already have GO instruction"))
	(maybe-insert-or-fix-jump-instr block to-block "LABEL"))))

(defun pop-labels-env (lambda-ssa)
  (let ((ssa-env (lambda-ssa-env lambda-ssa)))
    (pop (ssa-env-labels ssa-env))))

(defun push-labels-env (lambda-ssa)
  (let ((ssa-env (lambda-ssa-env lambda-ssa)))
    (push (make-slabels) (ssa-env-labels ssa-env))))

(defun label-ssa-block (block label &optional (emit t))
  (when emit
    (emit-ir-first (make-ssa-label :label label) block))
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
(defun emit-ir (ssa block)
  (error-if-touch-ir)
  (if (and (ssa-block-uncond-jump block)
	   (ssa-go-p (first (ssa-block-ir block))))
      (print-debug "Skipping dead code " ssa)
      (progn
	(if (ssa-form-p ssa)
	    (progn
	      (setf (ssa-form-index ssa) *ir-index-counter*)
	      (incf *ir-index-counter*))
	    (break))
	(if (ssa-block-ir-last-cons block)
	    (progn
	      (let ((new (cons ssa nil)))
		(setf (cdr (ssa-block-ir-last-cons block)) new)
		(setf (ssa-block-ir-last-cons block) new)))
	    (progn
	      (setf (ssa-block-ir block) (cons ssa nil))
	      (setf (ssa-block-ir-last-cons block) (ssa-block-ir block)))))))

(defun emit-ir-first (ssa block)
  (error-if-touch-ir)
  ;; if there is no IR we must use EMIT-IR  because how we operate with IR code
  (if (null (ssa-block-ir block))
      (emit-ir ssa block)
      (progn
	(if (ssa-form-p ssa)
	    (progn
	      (setf (ssa-form-index ssa) *ir-index-counter*)
	      (incf *ir-index-counter*))
	    (break))
	(setf (ssa-block-ir block)
	      (cons ssa (ssa-block-ir block))))))

(defun ssa-block-ir-last-instr (block)
  (car (last (ssa-block-ir block))))

(defun ssa-block-ir-first-instr (block)
  (car (ssa-block-ir block)))

(defun ssa-block-last-instruction (block)
  (error-if-touch-ssa)
  (car (last (ssa-block-ssa block))))

(defun ssa-block-first-instruction (block)
  (error-if-touch-ssa)
  (car (ssa-block-ssa block)))

(defun ssa-block-first-index (block)
  (ssa-form-index (ssa-block-first-instruction block)))

(defun ssa-block-last-index (block)
  (ssa-form-index (ssa-block-last-instruction block)))

(defun is-last-instr-jump (block)
  (declare (optimize (debug 3)))
  (let ((last-instr (ssa-block-ir-last-instr block)))
    (and (or (ssa-go-p last-instr)
	     (ssa-if-p last-instr))
	 last-instr)))

(defun is-first-instr-label (block)
  (and (ssa-label-p (ssa-block-first-instruction block))
       (ssa-block-first-instruction block)))

(defun remove-last-ir-instruction (block)
  (error-if-touch-ir)
  (setf (ssa-block-ir block) (butlast (ssa-block-ir block))))

(defun remove-last-instruction (block)
  (error-if-touch-ssa)
  (setf (ssa-block-ssa block) (butlast (ssa-block-ssa block))))

(defun emit-single-return-sequence (place block)
  (emit-ir (make-ssa-load :to (make-return-value-place :index 0) :from place) block)
  (emit-ir (make-ssa-return) block))

;;; FIXME, set block LABEL fieds
;;; FIXME, we are allways moving value to specific place, we can omit that and use existing place (and force it to register)
(defun emit-if-node-ssa (if-node lambda-ssa leaf place block)
  (let* ((test-node (clcomp::if-node-test-form if-node))
	 (test-place (generate-virtual-place))
	 (block (emit-ssa test-node lambda-ssa nil test-place block))
	 (false-block (make-new-ssa-block lambda-ssa))
	 (true-block (make-new-ssa-block lambda-ssa))
	 (next-block (unless leaf (make-new-ssa-block lambda-ssa))))
    ;; we can just set successor if this block is last block
    ;; if not set UNCOND-JUMP
    (if (= (ssa-block-index block)
	   (ssa-block-index (car (last (lambda-ssa-blocks lambda-ssa)))))
	(ssa-connect-blocks block false-block)
	(insert-block-unconditional-jump block false-block))
    (ssa-add-block lambda-ssa false-block)
    (ssa-add-block lambda-ssa true-block)
    (let ((true-form-ret-block (emit-ssa (clcomp::if-node-true-form if-node)
					 lambda-ssa leaf place true-block))
	  (false-form-ret-block (emit-ssa
				 (clcomp::if-node-false-form if-node)
				 lambda-ssa leaf place false-block)))
      (unless leaf
	(if (blocks-have-same-index true-block true-form-ret-block)
	    (ssa-maybe-connect-blocks true-block next-block)
	    (ssa-maybe-connect-blocks true-form-ret-block next-block))
	(if (blocks-have-same-index false-block false-form-ret-block)
	    (insert-block-unconditional-jump false-block next-block)
	    (insert-block-unconditional-jump false-form-ret-block next-block))))
    ;; it's important to add next-block after emiting IR for false and true blocks
    ;; if any of false/true blocks creates new blocks than we keep good block order
    (unless leaf
      (ssa-add-block lambda-ssa next-block))
    (insert-block-conditional-jump block true-block)
    (let ((true-block-label (generate-label-for-string "TBLOCK"))
	  (false-block-label (generate-label-for-string "FBLOCK")))
      (label-ssa-block true-block true-block-label)
      (label-ssa-block false-block false-block-label)
      (emit-ir (make-ssa-if
		:test test-place
		:true-block (ssa-block-index true-block)
		:true-block-label true-block-label
		;; :false-block-label false-block-label
		;; because false block is always next in order so it's always SUCC 
		:false-block-label nil)
	       block))
    next-block))

(defun maybe-emit-direct-load (node lambda-ssa leaf place block)
  (etypecase node
    (clcomp::immediate-constant-node
     (emit-ir (make-ssa-load :to place
    			     :from node) block)
     (when leaf
       (emit-single-return-sequence place block))
     block)
    (clcomp::lexical-var-node
     (emit-ir (make-ssa-load :to place
    			     :from (make-var-place :name (clcomp::lexical-var-node-name node))) block)
     (when leaf
       (emit-single-return-sequence place block))
     block)
    ;; (clcomp::values-node
    ;;  (error "not implemented"))
    (t (emit-ssa node lambda-ssa leaf place block))))

;;; FIXME, there is more here
(defun make-direct-place-or-nil (node)
  (etypecase node
    (clcomp::immediate-constant-node node)
    (clcomp::lexical-var-node (make-var-place :name (clcomp::lexical-var-node-name node)))
    (t nil)))

;;; FIXME, fun can be CLOSURE or LAMBDA
;;; FIXME, we don't need to fill all return places, if we need only one supply NIL for rest of VOP return values
(defun emit-call-node-ssa (node lambda-ssa leaf place block)
  (declare (optimize (debug 3) (speed 0)))
  (let* ((fun (clcomp::call-node-function node))
	 (arguments (clcomp::call-node-arguments node))
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
	;; FIXME, maybe p is already virtual place, no need for additional move
	(emit-ir (make-ssa-load :to (make-argument-place :index arg-index) :from p) block)
	(incf arg-index)))
    (emit-ir (make-ssa-load :to (make-argument-count-place)
			    :from (make-immediate-constant :constant (clcomp::fixnumize arguments-count))) block)
    (let ((fixup (make-compile-function-fixup :name (generate-fixup-symbol)
					      :function fun)))
      (emit-ir (make-ssa-load :to (make-function-value-place) :from fixup) block)
      (lambda-add-fixup fixup lambda-ssa))
    (if (null place)
	(progn
	  (emit-ir (make-ssa-unknown-values-fun-call :fun fun)
		   block)
	  (when leaf
	    (emit-ir (make-ssa-multiple-return) block))
	  block)
	(progn
	  (emit-ir (make-ssa-unknown-values-fun-call :fun fun) block)
	  (typecase place
	    (mvb-place
	     (mvb-bind-to-fun-call place block)
	     (emit-ir (make-ssa-mvb-bind :places (mvb-place-var-places place)) block))
	    (otherwise
	     (emit-ir (make-ssa-load :to place :from (make-return-value-place :index 0)) block)))
	  block))))

(defun emit-vop-node-ssa (node lambda-ssa leaf place block)
  (let ((ret-vals (length (clcomp::vop-res (clcomp::get-vop (clcomp::vop-node-vop node)))))
	(arguments (clcomp::vop-node-arguments node))
	(args-places nil))
    (dolist (arg arguments)
      (let ((direct-place (make-direct-place-or-nil arg)))
	(if direct-place
	    (push direct-place args-places)
	    (let* ((place (generate-virtual-place))
		   (new-block (maybe-emit-direct-load arg lambda-ssa nil place block)))
	      (setf block new-block)
	      (push place args-places)))))
    (setf args-places (reverse args-places))
    ;; FIXME, much of opportunities here to optimize VOP call, immediates, etc etc
    (dolist (p args-places)
      ;; FIXME, maybe p is already virtual place, no need for additional move
      ;; or just leave like this to not complicate and remove unnecessary moves and places later in peephole optimazer
      (emit-ir (make-ssa-load :to (generate-virtual-place) :from p) block))
    (cond (leaf
	   (emit-ir (make-ssa-vop :return-values (generate-return-places ret-vals) :args args-places) block)
	   (emit-ir (make-ssa-return-values :values-count ret-vals) block))
	  (place
	   (typecase place
	     (mvb-place
	      (emit-ir (make-ssa-vop :return-values (mvb-place-var-places place) :args args-places) block))
	     (t
	      ;; FIXME, here we use PLACE as first ret value and we generate missing values, fix VOP so it can receive NIL as return reg
	      (emit-ir (make-ssa-vop :return-values (cons place (generate-return-places (- ret-vals 1))) :args args-places) block))))
	  (t
	   ;; FIXME, fix VOP to maybe not use any return registers, do we have side effects onlu VOPS ?
	   (emit-ir (make-ssa-vop :return-values (generate-return-places ret-vals) :args args-places) block)))
    block))

(defun emit-lexical-binding-node-ssa (node lambda-ssa leaf block)
  (declare (ignore leaf))
  (let ((lvar (clcomp::lexical-binding-node-name node))
	(form (clcomp::lexical-binding-node-form node)))
    (maybe-emit-direct-load form lambda-ssa nil (make-var-place :name lvar) block)))

(defun emit-let-node-ssa (node lambda-ssa leaf place block)
  (dolist (n (clcomp::let-node-bindings node))
    (let ((new-block (emit-lexical-binding-node-ssa n lambda-ssa nil block)))
      (setf block new-block)))
  (emit-ssa (clcomp::let-node-form node) lambda-ssa leaf place block))

;;; FIXME, we can omit SSA-VALUE when it's not leaf ?
(defun emit-immediate-node-ssa (node lambda-ssa leaf place block)
  (declare (ignore lambda-ssa))
  (if place
      (emit-ir (make-ssa-load :to place :from node) block)
      (if leaf
	  (emit-single-return-sequence node block)
	  (emit-ir (make-ssa-value :value node) block)))
  block)

;;; FIXME, we can omit SSA-VALUE when it's not leaf ?
(defun emit-lexical-var-node-ssa (node lambda-ssa leaf place block)
  (declare (ignore lambda-ssa))
  (if place
      (emit-ir (make-ssa-load :to place :from (make-var-place :name (clcomp::lexical-var-node-name node))) block )
      (if leaf
	  (emit-single-return-sequence (make-var-place :name (clcomp::lexical-var-node-name node))  block)
	  (emit-ir (make-ssa-value :value
				   (make-var-place :name (clcomp::lexical-var-node-name node))) block)))
  block)

(defun emit-progn-node-ssa (node lambda-ssa leaf place block)
  (do* ((forms (clcomp::progn-node-forms node) (cdr forms))
	(form-node (car forms) (car forms)))
       ((null forms) block)
    (if (cdr forms)
	(setf block (emit-ssa form-node lambda-ssa nil nil block))
	(setf block (emit-ssa form-node lambda-ssa leaf place block)))))

(defun emit-tagbody-node-ssa (node lambda-ssa leaf place block)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (push-labels-env lambda-ssa)
  (let ((labels-blocks (make-hash-table))
	(current-block block))
    (dolist (node (clcomp::tagbody-node-forms node))
      (when (clcomp::label-node-p node)
	(let ((label-block (make-new-ssa-block lambda-ssa)))
	  (ssa-add-block-label lambda-ssa label-block (clcomp::label-node-label node))
	  (when (gethash (clcomp::label-node-label node) labels-blocks)
	    (error "Duplicate label in tagbody form"))
	  (setf (gethash (clcomp::label-node-label node) labels-blocks) label-block))))
    (dolist (form-node (clcomp::tagbody-node-forms node))
      (if (clcomp::label-node-p form-node)
	  (let ((lblock (gethash (clcomp::label-node-label form-node) labels-blocks)))
	    ;; connect previus block with this label block only if there is no direct jump from previous to other block
	    (ssa-add-block lambda-ssa lblock)
	    ;; when current block doesn't have GO just connect blocks
	    (when (null (ssa-block-uncond-jump current-block))
	      (ssa-connect-blocks current-block lblock))
	    ;; we are inserting SSA-LABEL with SSA-ADD-BLOCK-LABEL
	    ;; (emit-ir (make-ssa-label :label (label-node-label form-node))
	    ;; 	  lblock)
	    (setf current-block lblock))
	  (setf current-block (emit-ssa form-node lambda-ssa nil nil current-block)))
      (setf block current-block))
    (pop-labels-env lambda-ssa)
    (if place
	(emit-ir (make-ssa-load :to place :from (make-immediate-constant :constant clcomp::*nil*)) block)
	(when leaf
	  (emit-single-return-sequence (make-immediate-constant :constant clcomp::*nil*) block )
	  ;; FIXME, need this ?
	  ;; (emit lambda-ssa (make-immediate-constant :constant *nil*) block)
	  ))
    current-block))

;;; FIXME, delete unreachable code, look at EMIT function, it will ignore instructions after SSA-GO
(defun emit-go-node-ssa (node lambda-ssa leaf place block)
  (declare (ignore leaf place))
  (let ((label-name (clcomp::label-node-label (clcomp::go-node-label-node node))))
    (insert-block-unconditional-jump block (ssa-find-block-by-index lambda-ssa
								    (ssa-find-block-index-by-label lambda-ssa label-name)))

    ;; we are emiting SSA-GO with INSERT-BLOCK-UNCONDITIONAL-JUMP
    ;; (emit-ir (make-ssa-go :label label-name) block)
    ;; reset successor to nil, if we have GO direct successor in this block is never active
    (setf (ssa-block-succ block) nil)
    (make-new-ssa-block lambda-ssa)))

(defun emit-setq-node-ssa (node lambda-ssa leaf place block)
  (let ((new-block (maybe-emit-direct-load (clcomp::setq-node-form node)
					   lambda-ssa leaf
					   (make-var-place
					    :name (clcomp::lexical-var-node-name
						   (clcomp::setq-node-var node)))
					   block)))
    (if place
	(maybe-emit-direct-load (clcomp::setq-node-var node) lambda-ssa leaf place new-block))
    new-block))

(defun emit-lambda-arguments-ssa (arguments lambda-ssa block)
  (declare (ignore lambda-ssa))
  (emit-ir (make-arg-check :arg-count (length arguments)) block)
  (let ((index 0))
    (dolist (argument arguments)
      (etypecase argument
	(clcomp::lexical-var-node
	 (emit-ir (make-ssa-load :to (make-var-place :name (clcomp::lexical-var-node-name argument))
					 :from (make-rcv-argument-place :index index))
	       block)))
      (incf index))))

(defun rip-relative-node-to-fixup (node)
  (let ((fixup-sym (generate-fixup-symbol)))
    (etypecase node
      (clcomp::lambda-node (make-local-component-fixup :name fixup-sym))
      (clcomp::load-time-value-node (make-load-time-eval-fixup :name fixup-sym))
      (clcomp::fun-rip-relative-node (make-compile-function-fixup :name fixup-sym
							  :function (clcomp::fun-rip-relative-node-form node)))
      (clcomp::compile-time-constant-node (make-compile-time-constant-fixup :name fixup-sym
								    :form (clcomp::compile-time-constant-node-form node))))))

(defun emit-rip-relative-node-ssa (node lambda-ssa leaf place block)
  (let ((fixup (rip-relative-node-to-fixup node)))
    (typecase node
      (clcomp::load-time-value-node
       (add-sub-lambda lambda-ssa (lambda-construct-ssa (clcomp::load-time-value-node-node node)) fixup))
      (clcomp::lambda-node
       (add-sub-lambda lambda-ssa (lambda-construct-ssa node) fixup)))
    (lambda-add-fixup fixup lambda-ssa)
    (if place
	(emit-ir (make-ssa-load :to place :from fixup) block)
	(if leaf
	    (emit-single-return-sequence fixup block)
	    ;;; FIXME, we can omit SSA-VALUE node here ???
	    (emit-ir (make-ssa-value :value fixup) block)))
    block))

(defun emit-m-v-b-node-ssa (node lambda-ssa leaf place block)
  (declare (optimize (debug 3) (speed 0)))
  (let ((mvb-place (make-mvb-place :var-places (mapcar (lambda (b)
							 (make-var-place :name (clcomp::m-v-b-binding-node-name b)))
						       (clcomp::m-v-b-node-bindings node)))))
    (emit-ssa (clcomp::m-v-b-node-form node) lambda-ssa nil mvb-place block)
    (emit-ssa (clcomp::m-v-b-node-body node) lambda-ssa leaf place block)))


;;; FIXME, should we generate proper code here to generate stack return values
;;; or do that in translating phase ?
(defun emit-values-node-ssa (node lambda-ssa leaf place block)
  (let ((ret-index 0))
    (dolist (form (clcomp::values-node-forms node))
      (if leaf
	  ;; what if PLACE is existing ?? or we don't care ?
	  (maybe-emit-direct-load form lambda-ssa nil (make-return-value-place :index ret-index) block)
	  (if place
	      (typecase place
		(mvb-place
		 (let ((var-place (nth ret-index (mvb-place-var-places place))))
		   (if var-place
		       (maybe-emit-direct-load form lambda-ssa nil var-place block)
		       (emit-ssa form lambda-ssa nil nil block))))
		(otherwise (if (zerop ret-index)
			       (maybe-emit-direct-load form lambda-ssa nil place block)
			       (emit-ssa form lambda-ssa nil nil block))))
	      (emit-ssa form lambda-ssa nil nil block)))
      (incf ret-index)))
  (when leaf
    (emit-ir (make-ssa-multiple-return :count (length (clcomp::values-node-forms node))) block))
  block)

;;; FIXME, don't macroexpand BLOCK to TAGBODY, implement BLOCK directly in IR
;;; currently macroexpanding BLOCK to TAGBODY doesn't work for (return-from FUN (values 1 2))
;; simple fix is to macroexpand with MULTIPLE-VALUE-LIST ??

;;; FIXME, VALUES compilation: if VALUES is at FUNCTION return position then we need to use known registers/stack positions
;;; if other cases we can compile VALUES as multiple LET bindings
(defun emit-ssa (node lambda-ssa leaf place block)
  (etypecase node
    (clcomp::if-node (emit-if-node-ssa node lambda-ssa leaf place block))
    (clcomp::call-node (emit-call-node-ssa node lambda-ssa leaf place block))
    (clcomp::vop-node (emit-vop-node-ssa node lambda-ssa leaf place block))
    (clcomp::let-node (emit-let-node-ssa node lambda-ssa leaf place block))
    (clcomp::progn-node (emit-progn-node-ssa node lambda-ssa leaf place block))
    (clcomp::lexical-var-node (emit-lexical-var-node-ssa node lambda-ssa leaf place block))
    (clcomp::immediate-constant-node (emit-immediate-node-ssa node lambda-ssa leaf place block))
    (clcomp::tagbody-node (emit-tagbody-node-ssa node lambda-ssa leaf place block))
    (clcomp::go-node (emit-go-node-ssa node lambda-ssa leaf place block))
    (clcomp::setq-node (emit-setq-node-ssa node lambda-ssa leaf place block))
    (clcomp::rip-relative-node (emit-rip-relative-node-ssa node lambda-ssa leaf place block))
    (clcomp::m-v-b-node (emit-m-v-b-node-ssa node lambda-ssa leaf place block))
    (clcomp::values-node (emit-values-node-ssa node lambda-ssa leaf place block))))


(defun fix-cond-jump-index (from-block to-block)
  (declare (optimize (debug 3)))
  (let ((last-instr (ssa-block-ir-last-instr from-block)))
    (unless (ssa-if-p last-instr)
      (error "Last instruction is not SSA-IF"))
    (setf (ssa-if-true-block last-instr) (ssa-block-index to-block))
    (let ((label (ssa-block-label to-block)))
      (unless label
	(print-debug  "FIX-COND-JUMP-INDEX: Creating new label for block " (ssa-block-index to-block))
	(setf label (generate-label-for-string "IF-FBLOCK") )
	(label-ssa-block to-block label))
      (setf (ssa-if-true-block-label last-instr) label))))

(defun maybe-insert-or-fix-jump-instr (from-block to-block &optional (slabel "IF-FBLOCK"))
  (declare (optimize (debug 3)))
  (let ((label (ssa-block-label to-block)))
    (unless label
      (print-debug  "MAYBE-INSERT-OR-FIX-JUMP-INSTR: Creating new label for block " (ssa-block-index to-block))
      (setf label (generate-label-for-string slabel) )
      (label-ssa-block to-block label))
    (let ((jump-instruction (is-last-instr-jump from-block)))
      (if jump-instruction
	  (progn
	    (print-debug "Fixing JUMP instruction in block " (ssa-block-index from-block))
	    (if (ssa-go-p jump-instruction)
		(setf (ssa-go-label jump-instruction) label)
		(setf (ssa-if-false-block-label jump-instruction) label)))
	  (progn
	    (print-debug "Fixing block jump, emiting SSA-GO for block " (ssa-block-index from-block))
	    (emit-ir (make-ssa-go :label label) from-block))))))


(defun lambda-ssa-fix-ir-indexes (lambda-ssa)
  (error-if-touch-ir)
  (let ((ir-index 0))
    (dolist (b (lambda-ssa-blocks lambda-ssa))
      (dolist (ir (ssa-block-ir b))
	(setf (ssa-form-index ir) ir-index)
	(incf ir-index *instr-offset*))))
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

;; (defun fill-blocks-predecessors (lambda-ssa)
;;   (dolist (block (lambda-ssa-blocks lambda-ssa))
;;     (let ((block-index (ssa-block-index block))
;; 	  (succ (ssa-block-succ block))
;; 	  (uncond-jump (ssa-block-uncond-jump block))
;; 	  (cond-jump (ssa-block-cond-jump block)))
;;       (when (and succ uncond-jump)
;; 	(error "Can't have successor and unconditioned jump"))
;;       (dolist (bindex (remove nil (list succ uncond-jump cond-jump)))
;; 	(let ((sblock (ssa-find-block-by-index lambda-ssa bindex)))
;; 	  (push block-index (ssa-block-predecessors sblock)))))))

(defun ssa-write-variable (place block env)
  (declare (ignore env))
  (let ((vplace (generate-virtual-place "V-")))
    (set-block-def block (named-place-name place) vplace)))

;;; we need to use reduced value here
(defun try-remove-trivial-phi (phi block lambda-ssa)
  (declare (optimize (debug 3) (speed 0)))
  (let ((same nil)
	(phi-place (phi-place phi)))
    (dolist (operand (phi-operands phi))
      (let ((operand (get-maybe-reduced-place operand)))
	(cond ((or (eql operand same)
		   (eq operand (phi-place phi))))
	      ((not (null same))
	       (return-from try-remove-trivial-phi phi))
	      (t (setf same operand)))))
    (ssa-block-replace-phi block phi-place same)
    ;; FIXME, when adding replacement, if replacement is new PHI then we need to fix PHI operand usages
    (add-phi-value-replacement phi same lambda-ssa)
    (let ((phi-usages (get-phi-connections phi-place lambda-ssa)))
      (when phi-usages
	(dolist (uphi phi-usages)
	  ;; to prevent endless recursion skip PHI's that are already reduced to value
	  (unless (get-phi-place-reduced-value (phi-place uphi))
	    (replace-phi-operand uphi phi-place same)
	    (try-remove-trivial-phi uphi
				    (ssa-find-block-by-index lambda-ssa (phi-block-index uphi))
				    lambda-ssa)))))
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
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((predecessors (ssa-block-predecessors block)))
    (if (not (ssa-block-sealed block))
	(let* ((phi-place (generate-phi-place lambda-ssa))
	       (phi (make-phi :incomplete t :variable place
			      :place phi-place :block-index (ssa-block-index block))))
	  (ssa-block-add-phi block phi)
	  (set-block-def block (named-place-name place) phi-place))
	(when predecessors
	  (if (= (length predecessors) 1)
	      (set-block-def block (named-place-name place)
			     (ssa-read-variable place (ssa-find-block-by-index lambda-ssa (first predecessors)) lambda-ssa))
	      (let* ((phi-place (generate-phi-place lambda-ssa))
		     (phi (make-phi :variable place :place phi-place
				    :block-index (ssa-block-index block))))
		(ssa-block-add-phi block phi)
		(set-block-def block (named-place-name place) phi-place)
		(phi-add-operands place phi predecessors block lambda-ssa)
		phi-place))))))

(defun ssa-read-variable (place block lambda-ssa)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (or (get-block-def block (named-place-name place)) 
      (read-variable-recursive place block lambda-ssa)))

(defun transform-write (place block lambda-ssa)
  (typecase place
    (named-place
     (ssa-write-variable place block lambda-ssa))
    (t place)))

(defun transform-read (place block lambda-ssa)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
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
					  :true-block (ssa-if-true-block ir)
					  :true-block-label (ssa-if-true-block-label ir)
					  :false-block-label (ssa-if-false-block-label ir)))
		     (t ir))))
	(push irssa ssa-ir)))
    (setf (ssa-block-ssa b) (reverse ssa-ir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remove not needed PHI in a case of irreducible loop

(defun transpose-graph (successors-map)
  (let ((transposed-successors-map (make-hash-table)))
    (dolist (index (clcomp::hash-keys successors-map))
      (dolist (succ-index (gethash index successors-map))
	(push index (gethash succ-index transposed-successors-map))))
    transposed-successors-map))


(defun compute-scc (successors nodes)
  (labels ((fill-order (node visited stack)
	     (setf (gethash node visited) t)
	     (dolist (snode (gethash node successors))
	       (unless (gethash node visited)
		 (fill-order snode visited stack)))
	     (vector-push-extend node stack))
	   (dfs-util (node visited adj result)
	     (setf (gethash  node visited) t)
	     (push node (car result))
	     (dolist (n (gethash node adj))
	       (unless (gethash n visited)
		 (dfs-util n visited adj result)))
	     result))
    (let* ((visited (make-hash-table))
	   (stack (make-array (length nodes) :adjustable t :fill-pointer 0))
	   (transposed (transpose-graph successors))
	   (res nil))
      (dolist (node nodes)
	(unless (gethash node visited)
	  (fill-order node visited stack)))
      (clrhash visited)
      (tagbody
       start
	 (if (= 0 (length stack))
	     (go end)
	     (progn
	       (let ((node (vector-pop stack)))
		 (unless (gethash node visited)
		   (push (car (dfs-util node visited transposed
					(cons nil nil)))
			 res))
		 (go start))))
       end)
      res)))

(defun lambda-ssa-find-and-replace-phis (lambda-ssa phi-places value)
  (dolist (sblock (lambda-ssa-blocks lambda-ssa))
    (dolist (phi-place phi-places)
      (when (ssa-block-maybe-replace-phi sblock phi-place value)
	(setf phi-places (remove phi-place phi-places))))
    (when (null phi-places)
      (return-from lambda-ssa-find-and-replace-phis)))
  (unless (null phi-places)
    (error "Can't find all PHI's to replace")))

 (defun replace-scc-by-value (phc scc-phis value lambda-ssa)
  (declare (optimize (debug 3) (speed 0)))
  (let ((phi-places nil))
    (dolist (phi-node scc-phis)
      (let ((phi-place (phc-get-place phc phi-node)))
	(push phi-place phi-places)
	(add-phi-value-replacement (phc-get-phi phc phi-place) value lambda-ssa)))
    (lambda-ssa-find-and-replace-phis lambda-ssa phi-places value)))

(defun process-scc (scc phc lambda-ssa)
  (declare (optimize (debug 3) (speed 0)))
  (when (> (length scc) 1)
    (let ((inner nil)
	  (outer-ops nil))
      (dolist (phi-node scc)
	(let ((is-inner t))
	  (let ((phi (phc-get-phi-by-node phc phi-node)))
	    (dolist (operand (phi-operands phi))
	      (when (not (find operand (mapcar (lambda (node)
						 (phc-get-place phc node))
					       scc)))
		(pushnew operand outer-ops)
		(setf is-inner nil)))
	    (when is-inner
	      (pushnew phi inner)))))
      (cond ((= 1 (length outer-ops))
	     (replace-scc-by-value phc scc (first outer-ops) lambda-ssa))
	    ((> (length outer-ops) 1)
	     (remove-redundant-phis inner phc lambda-ssa))))))

(defun remove-redundant-phis (nodes phc lambda-ssa)
  ;; TODO, we need to  apply topological-sort on phis SCC
  (dolist (scc (compute-scc (phis-connections-successors phc) nodes))
    (process-scc scc phc lambda-ssa)))

(defun collect-maybe-redundant-phis (lambda-ssa)
  (let (good-phis)
    (dolist (sblock (lambda-ssa-blocks lambda-ssa))
      (let ((phis (ssa-block-all-phis sblock)))
	(dolist (maybe-phi phis)
 	  (when (phi-p maybe-phi)
	    ;; (and (phi-p maybe-phi)
	    ;;      (some #'place-is-phi (phi-operands maybe-phi)))
	      t
	    (push maybe-phi good-phis)))))
    good-phis))

(defstruct phis-connections
  (successors (make-hash-table))
  (init 0)
  (identity (make-hash-table))
  (reverse-index (make-hash-table))
  (phis (make-hash-table))
  graphs
  sccs)

(defun phc-add-phi (phc place phi)
  (setf (gethash place (phis-connections-phis phc)) phi))

(defun phc-get-place (phc node)
  (gethash node (phis-connections-reverse-index phc)))

(defun phc-get-phi (phc place)
  (gethash place (phis-connections-phis phc)))

(defun phc-get-phi-by-node (phc node)
  (phc-get-phi phc  (phc-get-place phc node)))

(defun get-or-make-identity (phc identity)
  (let ((i (gethash identity (phis-connections-identity phc))))
    (or i
	(progn
	  (setf (gethash identity  (phis-connections-identity phc))
		(incf (phis-connections-init phc)))
	  (setf (gethash (phis-connections-init phc) (phis-connections-reverse-index phc)) identity)
	  (phis-connections-init phc)))))

(defun identity-phi (index phc)
  (let ((value (gethash index (phis-connections-reverse-index phc))))
    (and value
	 (phi-place-p value)
	 value)))

(defun make-places-connections (phis)
  "Make successors map for every PHI operand"
  (let ((phc (make-phis-connections)))
    (dolist (phi phis)
      (phc-add-phi phc (phi-place phi) phi)
      (let ((phi-index (get-or-make-identity phc  (phi-place phi))))
	(dolist (operand (phi-operands phi))
	  (when (place-is-phi operand)
	    (let ((op-index (get-or-make-identity phc operand)))
	      (push phi-index (gethash op-index (phis-connections-successors phc))))))))
    (setf (phis-connections-graphs phc)
	  (make-graphs-from-successors (phis-connections-successors phc)))
    phc))


(defun make-graphs-from-successors (successors)
  "Create list of graphs from list of phis"
  (labels ((all-nodes (successors)
	     (remove-duplicates (append (clcomp::hash-keys successors)
					(mapcan #'nconc (copy-tree (clcomp::hash-values successors))))))
	   (dfs (node links visited current)
	     (setf (gethash node visited) t)
	     (setf (gethash node current) t)
	     (dolist (ln (gethash node links))
	       (unless (gethash ln visited)
		 (dfs ln links visited current))))
	   (get-linked-nodes (successors)
	     (let ((predecessors (transpose-graph  successors))
		   (linked (make-hash-table)))
	       (dolist (k (clcomp::hash-keys successors))
		 (let ((sucs (gethash k successors))
		       (preds (gethash k predecessors)))
		   (setf (gethash k linked) (remove-duplicates (append sucs preds)))))
	       linked)))    
    (let ((nodes (all-nodes successors))
	  (links (get-linked-nodes successors))
	  (visited (make-hash-table))
	  (graphs nil))
      (dolist (node nodes)
	(unless (gethash node visited)
	  (let ((current (make-hash-table)))
	    (dfs node links visited current)
	    (push (clcomp::hash-keys current) graphs))))
      graphs)))

(defun optimize-redundant-phis (lambda-ssa)
  (declare (optimize (debug 3) (speed 0)))
  (let* ((phis (collect-maybe-redundant-phis lambda-ssa))
	 (phc (make-places-connections phis))
	 ;; FIXME, this is not topological sort, see paper
	 (indexes (sort (clcomp::hash-values (phis-connections-identity phc)) #'< )))
    (remove-redundant-phis indexes phc lambda-ssa)))

;;; END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun every-predecessor-processed-p (predecessor-indexes lambda-ssa)
  (dolist (pblock predecessor-indexes)
    (let ((block (ssa-find-block-by-index lambda-ssa pblock)))
      (unless (ssa-block-processed block)
	(return-from every-predecessor-processed-p nil))))
  t)

(defun maybe-seal-block (ssa-block lambda-ssa)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
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
    (unless (ssa-block-sealed block)
      (error "Block not seaeled !!"))    
    (setf (ssa-block-ir block) nil))
  lambda-ssa)

(defun set-block-virtuals (lambda-ssa)
  (dolist (block (lambda-ssa-blocks lambda-ssa))
    (setf (ssa-block-virtuals block)
	  (collect-block-virtuals block lambda-ssa))))

(defun fill-blocks-ordering (lambda-ssa)
  (clrhash  (lambda-ssa-block-order-index lambda-ssa))
  (let ((start 0))
    (dolist (block (lambda-ssa-blocks lambda-ssa))
      (setf (ssa-block-order block) start)
      (setf (gethash start (lambda-ssa-block-order-index lambda-ssa))  block)
      (incf start))))

(defun traverse-and-mark-accessible-blocks (block lambda-ssa visited)
  (unless (gethash (ssa-block-index block) visited)
    (setf (gethash (ssa-block-index block) visited) t)
    (dolist (sblock (ssa-block-successors block lambda-ssa))
      (traverse-and-mark-accessible-blocks sblock lambda-ssa visited))))

(defun maybe-remove-redundant-blocks (sblocks lambda-ssa)
  (declare (optimize (debug 3)))
  (dolist (sblock-cons sblocks)
    (let* ((sblock (car sblock-cons))
	   (sbtype (cdr sblock-cons))
	   (suc (first (ssa-block-successors-types-and-indexes sblock)))
	   (succ-index (cdr suc))
	   (predecessors-types (ssa-block-predecessors-types-and-indexes sblock lambda-ssa)))
      (unless
	  ;; if predecessor have our block in SUCC field and also have UNCOND-JUMP we can't fix order so we skip SBLOCK
	  ;; this shouldn't happen btw !!
	  (dolist (pcons predecessors-types)
	    (when (and (eq (cdr pcons) 'succ)
		       (ssa-block-uncond-jump (ssa-find-block-by-index lambda-ssa (car pcons))))
	      (print-debug (format t "Skipping to remove block ~A, SUCC + UNCOND-JUMP case from ~A" (ssa-block-index sblock)
				   (car pcons)))
	      (return t)))
	;; we are clear to remove our block
	(print-debug "Removing REDUNDANT BLOCK " (ssa-block-index sblock))
	;; replace predecessors jumps with empty block successor
	(dolist (pcons predecessors-types)
	  (let* ((pblock (car pcons))
		 (pblock-block (ssa-find-block-by-index lambda-ssa pblock))
		 (type (cdr pcons))
		 (succ-block (ssa-find-block-by-index lambda-ssa succ-index)))
	    (ecase type
	      (succ
	       (setf (ssa-block-uncond-jump pblock-block) succ-index)
	       (setf (ssa-block-succ pblock-block) nil)
	       (maybe-insert-or-fix-jump-instr pblock-block succ-block))
	      (cond-jump
	       (setf (ssa-block-cond-jump pblock-block) succ-index)
	       (fix-cond-jump-index pblock-block succ-block ))
	      (uncond-jump
	       (setf (ssa-block-uncond-jump pblock-block) succ-index)
	       (maybe-insert-or-fix-jump-instr pblock-block succ-block)))
	    (when (eq sbtype 'ssa-go))))
	;; remove block from list
	(setf (lambda-ssa-blocks lambda-ssa)
	      (delete (ssa-block-index sblock) (lambda-ssa-blocks lambda-ssa) :test (lambda (index b)
										      (= index (ssa-block-index b)))))
	(destroy-ssa-block  sblock lambda-ssa)
	;; fix predecessors on block which we connected to
	(let ((del-block-index (ssa-block-index sblock))
	      (succ-block (ssa-find-block-by-index lambda-ssa succ-index))
	      (new-predecessors (mapcar #'car predecessors-types)))
	  (setf (ssa-block-predecessors succ-block)
		(append new-predecessors
			(delete del-block-index (ssa-block-predecessors succ-block)))))))))

(defun is-block-redundant (sblock)
  "Decide if block can be deleted.
   Block can be deleted if there is LABEL instruction before SSA-VALUE or SSA-GO inst.
   Also block need to have only 1 successor"
  (when (= 1 (length (ssa-block-successors-indexes sblock)))
    (let ((last nil))
      (dolist (instr (ssa-block-ir sblock))
	(typecase instr
	  (ssa-label (if last
			 (error "Label after instruction")
			 (setf last 'ssa-label)))
	  (ssa-go (if (eq last 'ssa-go)
		      (error "Two SSA-GO instructions")
		      (setf last 'ssa-go)))
	  (ssa-value (if (eq last 'ssa-go)
			 (error "SSA-VALUE after SSA-GO instruction")
			 (setf last 'ssa-value)))
	  (t (return-from is-block-redundant nil))))
      (or last 'empty))))

(defun remove-redundant-blocks (lambda-ssa)
  "Sometimes we create a block with no IR, dettach predecessors and connect it to empty block successor.
 There is also case when block only contain one GO instruction, we can also delete this block and fix predecessors/successors connections"
  (let (maybe-redundant-blocks)
    (dolist (sblock (lambda-ssa-blocks lambda-ssa))
      (let ((redundant (is-block-redundant sblock)))
	(when redundant
	  (push (cons sblock redundant) maybe-redundant-blocks))))
    (when maybe-redundant-blocks
      (maybe-remove-redundant-blocks maybe-redundant-blocks lambda-ssa))))

(defun remove-not-accessible-blocks (lambda-ssa)
  (let* ((blocks (lambda-ssa-blocks lambda-ssa))
	 (entry-block (first blocks))
	 (visited (make-hash-table)))
    (traverse-and-mark-accessible-blocks entry-block lambda-ssa visited)
    (let ((new-blocks nil))
      (dolist (block blocks)
	(if (gethash (ssa-block-index block) visited)
	    (push block new-blocks)
	    (progn
	      (print-debug "Removing NOT-ACCESSIBLE BLOCK " (ssa-block-index block))
	      (dolist (sblock (ssa-block-successors block lambda-ssa))
		(when sblock
		  (when (gethash (ssa-block-index sblock) visited)
		    (setf (ssa-block-predecessors sblock)
			  (remove (ssa-block-index block) (ssa-block-predecessors sblock))))))
	      (destroy-ssa-block block lambda-ssa))))
      (setf (lambda-ssa-blocks lambda-ssa) (reverse new-blocks)))
    ;; remove empty blocks that are stil in LAMBDA-SSA-BLOCKS-INDEX
    (dolist (block (clcomp::hash-values (lambda-ssa-blocks-index lambda-ssa )))
      (unless (gethash (ssa-block-index block) visited)
	(dolist (index (ssa-block-successors-indexes block))
	  (let ((sblock (ssa-find-block-by-index lambda-ssa index)))
	    (when sblock
	      (setf (ssa-block-predecessors sblock)
		    (remove (ssa-block-index block) (ssa-block-predecessors sblock))))))
	(print-debug "Removing BLOCK " (ssa-block-index block))
	(destroy-ssa-block block lambda-ssa)))))

;;; FIXME, check this, remove GO when we change UNCOND-JUMP to SUCCESSOR
(defun maybe-fix-uncond-jumps-to-succ (lambda-ssa)
  (declare (optimize (debug 3)))
  (flet ((change-uncond-jump-to-succ (ssa-block)
	   ;; if last instruction is GO remove it
	   ;; if last instruction is IF set FALSE-BLOCK-LABEL to NIL
	   (let* ((last-instruction (ssa-block-ir-last-instr ssa-block)))
	     (typecase last-instruction
	       (ssa-go (remove-last-ir-instruction ssa-block))
	       (ssa-if (setf (ssa-if-false-block-label last-instruction) nil))
	       (t ;; (print-debug "CHANGE-UNCOND-JUMP-TO-SUCC, unknown last instruction")
		(error "Unknown last instruction")
		)))))
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
	  (cond ((and (null b1-succ)
		      b1-uncond-jump
		      b2
		      (= b1-uncond-jump (ssa-block-index b2)))
		 (setf (ssa-block-succ b1) b1-uncond-jump)
		 (setf (ssa-block-uncond-jump b1) nil)
		 (change-uncond-jump-to-succ b1)
		 ;; FIXME, remove GO or fix IF
		 (print-debug "Fixing UNCOND-JUMP to SUCC for " (ssa-block-index b1)))
		((and b1-succ
		      b2
		      (/= b1-succ (ssa-block-index b2)))
		 (error (format nil "Wrong SUCC index for BLOCK ~A" (ssa-block-index b1))))))))))

(defun check-predecessors (lambda-ssa)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((predecessors (make-hash-table)))
    (dolist (sblock (lambda-ssa-blocks lambda-ssa))
      (let ((block-successors (ssa-block-successors-indexes sblock)))
	(dolist (succ block-successors)
	  (push (ssa-block-index sblock) (gethash succ predecessors)))))
    (dolist (sblock (lambda-ssa-blocks lambda-ssa))
      (unless (equal (sort (ssa-block-predecessors sblock) #'<)
		     (sort (gethash (ssa-block-index sblock) predecessors) #'<))

	(error "We have error in predecessors field")))))


;;; FIXME, insert (GO TAG) at the end of instruction list in blocks that have UNCOND-JUMP
;;; FIXME, in SSA-IF form our JUMP is econded as INDEX, replace INDEX with BLOCK LABEL
(defun lambda-construct-ssa (lambda-node)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let* ((*ssa-block-counter* 0)
	 (*ir-index-counter* 0)
	 (*ssa-symbol-counter* 0)
	 (*error-on-ir-touch* nil)
	 (*error-on-ssa-touch* t)
	 (lambda-ssa (make-lambda-ssa))
	 (entry-block (make-new-ssa-block lambda-ssa)))
    (ssa-add-block lambda-ssa entry-block)
    (emit-ir (make-lambda-entry) entry-block)
    (emit-lambda-arguments-ssa (clcomp::lambda-node-arguments lambda-node ) lambda-ssa entry-block)
    (emit-ssa (clcomp::lambda-node-body lambda-node) lambda-ssa t nil entry-block)
    (remove-not-accessible-blocks lambda-ssa)
    (fill-blocks-ordering lambda-ssa)
    (when *optimize-redundant-blocks*
      ;; FIXME
      ;; we've added LABEL instruction so now we don't remove blocks
      (remove-redundant-blocks lambda-ssa))
    (check-predecessors lambda-ssa)
    (lambda-ssa-fix-ir-indexes lambda-ssa)
    (fill-blocks-ordering lambda-ssa)
    (maybe-fix-uncond-jumps-to-succ lambda-ssa)
    (setf *error-on-ssa-touch* nil)
    (construct-ssa lambda-ssa)
    (ssa-reset-original-ir lambda-ssa)
    (setf *error-on-ir-touch* t)
    (set-block-virtuals lambda-ssa)
    (when *optimize-redundant-phis*
      (optimize-redundant-phis lambda-ssa))
    (fill-blocks-ordering lambda-ssa)
    (compute-block-order lambda-ssa)
    lambda-ssa))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; block order

;;; 
;;; FIXME 
;;; Sometimes we can mark direct predecessor block as LOOP-END block
;;; remove LOOP-END from BLOCK in which LOOP-HEADER BLOCK is direct successor

#+nil
(defun select-and-mark-loop-end (block lambda-ssa)
  (let* ((header-block-index (ssa-block-index block))
	 (orders (mapcar (lambda (i)
			   (cons i (ssa-block-order (ssa-find-block-by-index lambda-ssa i))))
			 (ssa-block-predecessors block)))
	 (loop-end-indexes (sort orders (lambda (a b)
					  (> (cdr a) (cdr b))))))
    (dolist (pair loop-end-indexes)
      (let ((index (car pair))
	    (order (cdr pair)))
	;; we are not marking forward branch as loop-end
	(when (>= order
		 (ssa-block-order block))
	  (setf (ssa-block-is-loop-end (ssa-find-block-by-index lambda-ssa index)) t)
	  (push (cons index header-block-index) (lambda-ssa-loop-end-blocks lambda-ssa))
	  (push (cons header-block-index index) (lambda-ssa-loop-header-blocks lambda-ssa)))))))

#+nil
(defun mark-loop-end-blocks (lambda-ssa)
  (dolist (block (lambda-ssa-blocks lambda-ssa))
    (when (ssa-block-is-header block)
      (select-and-mark-loop-end block lambda-ssa))))

(defun cbo-visit (block previous-block lambda-ssa visited active)
  (declare (optimize (debug 3) (speed 0)))
  (print-debug "CBO-VISIT " (ssa-block-index block))
  (let* ((index (ssa-block-index block))
	 (b-visited (gethash index visited))
	 (b-active (gethash index active))
	 (successors (ssa-block-successors block lambda-ssa)))
    (unless b-visited
      (setf (gethash index visited) t))
    (when (= *break-block* (ssa-block-index block))
      (break))
    (if (> b-active 0)
	(progn
	  (print-debug "SSA-BLOCK-IS-HEADER " (ssa-block-index block) "END BLOCK " (ssa-block-index previous-block))
	  (lambda-ssa-add-loop-end-block (ssa-block-index block) (ssa-block-index previous-block) lambda-ssa)
	  (unless (ssa-block-is-header block)
	    (setf (ssa-block-is-header block) (get-block-unique-header-number)))
	  (incf (ssa-block-branch-to-count block)))
	(progn
	  (incf (gethash index active))
	  (dolist (sblock successors)
	    (cbo-visit sblock block lambda-ssa visited active))
	  (decf (gethash index active))))))

(defun trace-end-block (end-block header-block lambda-ssa)
  (declare (ignore end-block header-block lambda-ssa)))

(defun follow-end-blocks (loop-end-blocks header-block lambda-ssa)
  (dolist (end-block loop-end-blocks)
    (trace end-block header-block lambda-ssa)))

(defun mark-loop-blocks (lambda-ssa)
  (dolist (header-end-blocks (lambda-ssa-loop-header-blocks lambda-ssa))
    (let ((header-block (first header-end-blocks))
	  (loop-end-blocks (rest header-end-blocks)))
      (follow-end-blocks loop-end-blocks header-block lambda-ssa))))

;;; FIXME, this still doesn't implement block order from:
;;; "Linear Scan Register Allocation for the Java HotSpotTM Client Compiler"
;;; based on block loop index weight 
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
    (mark-loop-blocks lambda-ssa)
    ;; (mark-loop-end-blocks lambda-ssa)
    lambda-ssa))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun intervals-same-storage-p (i1 i2 &optional (compare-name t))
  (and
   (or (not compare-name)
       (eq (interval-name i1) (interval-name i2)))
   (equalp (interval-register i1) (interval-register i2))
   (equalp (interval-stack i1) (interval-stack i2))))

(defun make-interval-storage (interval)
  (let ((register (interval-register interval))
	(stack (interval-stack interval)))
    (if register
	(clcomp::make-reg-storage :register register)
	(clcomp::make-stack-storage :offset stack))))

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

(defun collect-block-virtuals (block lambda-ssa)
  (error-if-touch-ssa)
  (let ((reads nil)
	(writes nil))
    (dolist (ir (ssa-block-ssa block))
      (let* ((write (ssa-place (ssa-form-write-place ir)))
	     (orig-read (ssa-place (ssa-form-read-place ir)))
	     (read (maybe-get-simplified-phi-value orig-read block lambda-ssa)))
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

(defun ssa-block-predecessors-types-and-indexes (ssa-block lambda-ssa)
  (let ((res nil)
	(index (ssa-block-index ssa-block)))
    (dolist (bindex (ssa-block-predecessors ssa-block))
      (let* ((sblock (ssa-find-block-by-index lambda-ssa bindex))
	     (type (cond ((= (or (ssa-block-succ sblock) -1) index)
			  'succ)
			 ((= (or (ssa-block-uncond-jump sblock) -1) index)
			  'uncond-jump)
			 ((= (or (ssa-block-cond-jump sblock) -1) index)
			  'cond-jump)
			 (t
			  (error "Can't find our jump target")))))
	(push (cons bindex type) res)))
    res))

(defun ssa-block-successors-indexes (ssa-block)
  (remove-if #'null (list (ssa-block-succ ssa-block)
			  (ssa-block-cond-jump ssa-block)
			  (ssa-block-uncond-jump ssa-block))))

(defun ssa-block-successors-types-and-indexes (ssa-block)
  (remove-if (lambda (cons)
	       (null (cdr cons)))
	     (list (cons 'succ (ssa-block-succ ssa-block))
		   (cons 'cond-jump (ssa-block-cond-jump ssa-block))
		   (cons 'uncond-jump (ssa-block-uncond-jump ssa-block)))))

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

(defun live-add-phis-operands (live phis block lambda-ssa)
  (let ((block-virtuals (ssa-block-virtuals block))
	(block-defined (ssa-block-defined block)))
    (declare (ignorable block-defined))
    (dolist (phi phis)
      (when (phi-p phi)
	(dolist (orig-pop (phi-operands phi))
	  (let ((pop (maybe-get-simplified-phi-value orig-pop block lambda-ssa)))
	    ;; we should only use PHI operand that is live in block we are processing
	    ;; FIXME, what in case that we have reduced value of operand that was PHI ?
	    (when (and (not (find pop live :test #'equalp))
		       (or (find pop (first block-virtuals) :test #'equalp)
			   (find pop (second block-virtuals) :test #'equalp))
		       ;; (find pop block-defined :key #'cdr)
		       )
	      (push pop live)))))))
  live)

(defun make-intervals ()
  (make-hash-table))

(defun make-use-positions ()
  (make-hash-table))

(defun add-use-positions (use-positions virtual use-position)
  (let ((vname (get-place-name virtual)))
    (setf (gethash vname use-positions)
	  (cons use-position
		(gethash vname use-positions)))))

(defun get-use-positions (use-positions vname)
  (gethash vname use-positions))

(defun get-interval (intervals name)
  (gethash name intervals))

(defun add-interval (intervals interval)
  (setf (gethash (interval-name interval) intervals) interval))

(defun intervals-merge (s1 e1 s2 e2)
  (flet ((overlap (s1 e1 s2 e2)
	   (and (<= s1 (+ e2 *instr-offset*))
		(>= e1 (- s2 *instr-offset*)))))
    (when (overlap s1 e1 s2 e2)
      (list (min s1 s2)
	    (max e1 e2)))))

(defun add-range (intervals name start end)
  (let ((interval (get-interval intervals name)))
    (unless interval
      (setf interval (make-interval :name name))
      (add-interval intervals interval))
    (let ((active-range (first (interval-ranges interval))))
      (let ((merged-ranges (and active-range
				(intervals-merge start end
						 (range-start active-range)
						 (range-end active-range)))))
	(if merged-ranges
	    (progn
	      (setf (range-start active-range) (first merged-ranges))
	      (setf (range-end active-range) (second merged-ranges)))
	    
	    (push (make-range :start start
			      :end end)
		  (interval-ranges interval))))
      interval)))

(defun shorten-current-range (intervals place start)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let* ((name (get-place-name place))
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
      (let*((successor-blocks (ssa-block-successors block lambda-ssa))
	    (successors-live-in (mapcar #'ssa-block-live-in successor-blocks))
	    (live (merge-all-blocks-live-in successors-live-in)))

	(dolist (sb successor-blocks)
	  (let ((phis (ssa-block-all-phis sb)))
	    ;; TEST just to be sure that we are not processing PHI that are reduce to simple VALUE,
	    ;; remove later
	    (dolist (phi phis)
	      (when (phi-p phi)
		(let ((place (phi-place phi)))
		  (when (and (phi-place-reduced place)
			     (funcall (phi-place-reduced place)))
		    (error "This should now happen, we are using PHI that is reduced to normal PLACE")))))
	    (setf live (live-add-phis-operands live phis block lambda-ssa))))

	(when (ssa-block-first-instruction block)
	  (let ((start (ssa-form-index (ssa-block-first-instruction block)))
		(end (ssa-form-index (ssa-block-last-instruction block))))

	    (dolist (place live)
	      (let ((place-name (get-place-name place)))
		(add-range intervals place-name start end)))

	    (dolist (instr (reverse (ssa-block-ssa block)))
	      (let* ((instr-index (ssa-form-index instr))
		     ;; we can't have PHI-PLACE in write position
		     (write (ssa-place (ssa-form-write-place instr)))
		     (orig-read (ssa-place (ssa-form-read-place instr)))
		     (read (maybe-get-simplified-phi-value orig-read block lambda-ssa)))
		(when write
		  ;; FIXME, just one write to place that is never read
		  ;; we sure need to allocate register for this
		  (unless (shorten-current-range intervals write instr-index)
		    (add-range intervals (get-place-name write) instr-index instr-index))
		  (setf live (remove-from-live live write))
		  (add-use-positions use-positions write (make-use-write-pos :index instr-index)))
		;; FIXME, read is always adding RANGE
		;; when we then have WRITE we only shorten last RANGE
		(when read
		  (add-range intervals (get-place-name read) start instr-index)
		  (add-use-positions use-positions read (make-use-read-pos :index instr-index))
		  (pushnew read live :test #'equalp))))

	    (dolist (phi (ssa-block-all-phis block))
	      (when (phi-p phi)
		(let ((phi-place (phi-place phi)))
		  (setf live (remove-from-live live phi-place)))))

	    (when (ssa-block-is-header block)
	      (let* ((end-block-index (lambda-ssa-find-greatest-end-block lambda-ssa (ssa-block-index block)))
		     (end-block (ssa-find-block-by-index lambda-ssa end-block-index)))
		(dolist (lplace live)
		  (add-range intervals (get-place-name lplace) start (ssa-block-last-index end-block)))))))
	(setf (ssa-block-live-in block) live)))
    ;; (try-intervals-merge intervals)
    (add-intervals-use-positions intervals use-positions)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alternative Intervals building/Linear Scan from "Linear scan register allocation for Java HotSpot Client Compiler"

#+nil(defun insert-block-between (before-block after-block)
       (let ((new-block (make-new-ssa-block)) ;; FIXME, MAKE-NEW-SSA neds LAMBDA-SSA as argument
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; if *ALLOCATION-METHOD* is :simple don't split intervals
;;; other method is :split
(defparameter *allocation-method* :simple)

(defparameter *stack-offset* -1)

;;; FIXME, this is just for test
(defparameter *regs* (list :rax :rbx))

(defun get-stack-index ()
  (incf *stack-offset*))

(defstruct move block index from to)

(defstruct alloc unhandled active inactive handled
	   (per-name-handled (make-hash-table))
	   (block-intervals (make-hash-table))
	   (split-moves (make-hash-table)))

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

#+nil
(defun maybe-generate-move-load (from-interval to-interval block)
  (unless (intervals-same-storage-p from-interval to-interval nil)
    (make-move :block (ssa-block-index block)
	       :index index
	       :from  (make-interval-storage from-interval)
	       :to (make-interval-storage to-interval))))

;;; FIXME, generate split moves at the time when interval is splitted 
(defun generate-split-intervals-moves (alloc)
  (let ((moves (alloc-split-moves alloc)))
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
	      (setf current-interval intv)))))))
  alloc)

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

(defun get-interval-parent (interval alloc)
  (when (interval-parent interval)
    (let ((parent (alloc-get-handled-interval alloc (interval-parent interval))))
      (or parent
	  (error "Can't find interval parent")))))

;; (defun generate-split-move-for-child-interval (alloc parent child)
;;   (let ((load (make-ssa-load :index (1+ (interval-start child))
;; 			     :from  (make-interval-storage parent)
;; 			     :to (make-interval-storage child))))
;;     (let ((moves (alloc-split-moves alloc)))
;;       (push load (gethash (ssa-form-index load) moves)))))

(defun try-allocate-free-reg (current-interval current-position alloc)
  (declare (ignore current-position)
	   (optimize (speed 0) (debug 3) (safety 3)))
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
    (generate-split-intervals-moves alloc)
    alloc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-block-used-intervals (lambda-ssa alloc)
  (let ((intervals (sort (mapcar #'cdr (alloc-handled alloc)) #'< :key #'interval-start))
	(block-intervals (make-hash-table)))
    (dolist (block (lambda-ssa-blocks lambda-ssa))
      (let ((block-start (ssa-block-first-index block))
	    (block-end (ssa-block-last-index block)))
	(dolist (interval intervals)
	  (let* ((istart (interval-start interval))
		 (iend (interval-end interval)))
	    (if (or (and (>= istart block-start)
			 (<= istart block-end))
		    (and (>= iend block-start)
			 (<= iend block-end)))
		(push interval (gethash (ssa-block-index block) block-intervals))
		(when (> istart block-start)
		  (return)))))))
    (setf (alloc-block-intervals alloc) block-intervals)))

(defun get-first-or-last-intervals-in-block (block alloc what)
  (let ((block-intervals (gethash (ssa-block-index block) (alloc-block-intervals alloc)))
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
      (when (phi-p phi)
	(setf (gethash (named-place-name (phi-place phi)) h) phi)))
    h))

;;; FIXME
;;; when inserting resolve moves check to see if last instruction is JUMP
;;; if it is MOV's need to be inserted before JUMP

(defun resolve-phi-move (phi phi-interval sblock pblock pblock-intervals lambda-ssa alloc)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((operand-names (mapcar #'named-place-name (phi-operands phi))))
    (dolist (name operand-names)
      (let ((sint (gethash name pblock-intervals)))
	(when sint
	  (print 'phi-move)
	  (print (list 'maybe-generate-move-load sint phi-interval)))))))

(defun resolve-interval-block-move (sinterval pinterval)
  (print (list sinterval pinterval)))

(defun resolve-data-flow (lambda-ssa alloc)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (make-block-used-intervals lambda-ssa alloc)
  (dolist (sblock (lambda-ssa-blocks lambda-ssa))
    (let ((pindexes (ssa-block-predecessors sblock))
	  (sblock-phis (hash-block-phis sblock)))
      (when pindexes
	(let ((sintervals (get-first-or-last-intervals-in-block sblock alloc :first)))
	  (dolist (index pindexes)
	    (let* ((pblock (ssa-find-block-by-index lambda-ssa index))
		   (pintervals (get-first-or-last-intervals-in-block pblock alloc :last)))
	      (dolist (interval (clcomp::hash-values sintervals))
		(let ((maybe-phi (gethash (interval-name interval) sblock-phis)))
		  (if maybe-phi
		      (progn
			(resolve-phi-move maybe-phi interval sblock pblock pintervals lambda-ssa alloc))
		      (when (interval-parent interval)
			(let ((pinterval (gethash (interval-name interval) pintervals)))
			  ;; FIXME, what if split index is betwen blocks ?!?!
			  ;; test where child interval begins
			  (when (and pinterval
				     (/= (interval-number pinterval) (interval-number interval))
				     (not (intervals-same-storage-p interval pinterval nil)))
			    (resolve-interval-block-move interval pinterval))))))))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *generate-graph-fun* nil)

(defun generate-graph (ssa file)
  (when *generate-graph-fun*
    (funcall *generate-graph-fun* ssa file)))

(defun make-lssa (exp &optional file (optimize-blocks t) (optimize-phis t))
  (let ((*optimize-redundant-blocks* optimize-blocks)
	(*optimize-redundant-phis* optimize-phis))
    (let ((lssa (lambda-construct-ssa (clcomp::create-node (clcomp::clcomp-macroexpand exp)))))
      (when file
	(generate-graph lssa file))
      lssa)))

(defun make-lssa-intervals (exp)
  (let* ((lambda-ssa (lambda-construct-ssa (clcomp::create-node (clcomp::clcomp-macroexpand exp))))
	 (intervals (build-intervals lambda-ssa)))
    intervals))

(defun make-ssa-write-graph (exp &optional (optimize-blocks t) (optimize-phis t) (graph-name "default"))
  (let ((lambda-ssa (make-lssa exp optimize-blocks optimize-phis)))
    (generate-graph lambda-ssa graph-name)))

(defun test-ssa (exp &optional (graph-name "default"))
  (let* ((lambda-ssa (lambda-construct-ssa (clcomp::create-node (clcomp::clcomp-macroexpand exp))))
	 (_ (generate-graph lambda-ssa graph-name))
	 (intervals (build-intervals lambda-ssa))
	 (alloc (linear-scan intervals)))
    (declare (ignore _))
    (generate-graph lambda-ssa graph-name)
    (resolve-data-flow lambda-ssa alloc)
    (values lambda-ssa intervals alloc)))

(defun make-optimized-and-not-optimized (exp)
  (test-ssa exp "optimized")
  (let ((*optimize-redundant-blocks* nil))
    (test-ssa exp "not_optimized")))

;;; ChatGPT example of PHIS redundant elimination
#+nil
(make-lssa '(lambda (x y)
	     (let ((z (if (< x y) x y))
		   (w (if (< x y) x y)))
	       (+ z w))))
;;; Test case that currently doesn't work
#+nil
(test-ssa '(lambda (a)
	    (let ((c 0))
	      (tagbody 
	       bar
		 (setf c (+ c 1))
		 (when a (go bar)))
	      c)))
;;; this one triggers redundant phi's optimization
#+nil
(test-ssa '(lambda (a)
	    (tagbody
	       (when 1 (go third))
	     second
	       (print 1)
	     third
	       (when 2 (go second)))
	    a))
#+nil
(test-ssa '(lambda (a b)
	    (tagbody
	     start
	       (setf a 1)
	       (when b
		 (go end))
	     baz
	       (read a)
	       (if b
		   (go start)
		   (go end))
	     end)
	    a))

;;; maybe we can trigger reduced PHI here ?
#+nil
(lambda (x)
  (tagbody
   bla
     (if x
	 (progn
	   (setf x (+ x 20))
	   (go while))
	 (go exit))
   while
     (tagbody
      start
	(when (> x 1)
	  (setf x (+ x 10))
	  (go bla)))
   exit)
  x)


;;; triggets stack overflow
;;; fixed with WHEN macro bug fix but this will be triggered somewhere else
#+nil
(generate  (make-lssa  '(lambda (a b)
			 (tagbody
			  start
			    (setf a 1)
			    (when b (print 10)
				  (go end))
			  baz
			    (read a)
			    (if b
				(go start)
				(go end))
			  end)
			 a)))

;;; notes
;;; * kad se interval zavrsava negde u istoj tacki moze da pocne drugi interval ako se tu definise nova varijabla, samo mora da seobrati paznja na redosled
;;; * kad resavamo phi, insertujemo move na kraju prethodnog bloka, mozda treba da napravimo novi blok 

;;; FIXME
;; 
;;; triggers endless loop
#+nil
(test-ssa '(lambda (a)
            (dotimes (i a)
              (dotimes (c i)
                (print 1)))))

;;; sometimes we have COND-JUMP that jumps to BLOCK that is next in order

;; throws error
#+nil
(test-ssa '(lambda (a)
	    (dolist (l a)
	      (dolist (g l)
		(print l)))))


;;; SSA, blocks order
;;; sometimes we have COND-JUMP that jumps to BLOCK that is next in order (when emiting assembly code we can do IF-NOT and in that way just emit one JUMP instead of TWO)
;; 
;;; sometimes we have UNCOND-JUMP (in SSA-IF) form that jumps to next BLOCK in order


;;; cl-dot, we are drawing this incorrectly, order is not accurate
#+nil
(test-ssa '(lambda (x a)
	    (tagbody 
	       (go end)
	     x
	       (setf x (+ 1 x))
	       (go real-end)
	     y
	       (setf x (+ 2 x))
	       (go real-end)
	     end
	       (if a
		   (go x)
		   (go y))
	     real-end)
	    x))

;;; FIXME
;;; there is bug when removing redundant blocks, we are removing necessary blocks
;;; There is error in BUILD-INTERVALS here

#+nil
(test-ssa '(lambda (x a)
	    (tagbody foo
	       (tagbody 
		  (go end)
		x
		  (setf x (+ 1 x))
		  (go real-end)
		y
		  (setf x (+ 2 x))
		  (go real-end)
		end
		  (if a
		      (go x)
		      (go y))
		real-end)
	       (go foo))))
#+nil
(test-ssa '(lambda (x a)
	    (tagbody foo
	       (print x)
	       (go foo))))


#+nil
(make-lssa '(lambda (a)
	     (tagbody 
		(go foo)
	      a1
		(print 1)
	      a2 
		(print 2) 
	      a3
		(print 3)
	      foo
		(if a 
		    (go a1)
		    (go a2))
	      z)) "default")
