(in-package :clcomp)
(declaim (optimize (speed 0) (debug 3)))

(defstruct place)
(defstruct (argument-place (:include place)) index)
(defstruct (return-value-place (:include place)) index)
(defstruct (temp-place (:include place)) name)
(defstruct (var-place (:include place)) name)

(defstruct slabels alist)
(defstruct ssa-env labels)
(defstruct lambda-ssa blocks (env (make-ssa-env)))
(defstruct ssa-block index ir succ jump)

(defstruct ssa-form index)
(defstruct (ssa-load (:include ssa-form)) to from)
(defstruct (ssa-go (:include ssa-form)) label)
(defstruct (ssa-label (:include ssa-form)) label)

(defstruct (ssa-fun-call (:include ssa-form)) fun)
(defstruct (ssa-unknown-values-fun-call (:include ssa-form)) fun)
(defstruct (ssa-known-values-fun-call (:include ssa-form)) fun)
(defstruct (ssa-if (:include ssa-form)) test true-block)

(defun generate-temp-place ()
  (let ((counter (load-time-value (cons 0 nil))))
    (incf (car counter))
    (make-temp-place :name (make-symbol (concatenate 'string "T-" (write-to-string (car counter)))))))

(defparameter *ssa-block-counter* 0)
(defun make-new-ssa-block ()
  (prog1
      (make-ssa-block :index *ssa-block-counter*)
    (incf *ssa-block-counter*)))

(defun ssa-add-block (lambda-ssa block)
  (push block (lambda-ssa-blocks lambda-ssa)))

(defun ssa-connect-blocks (b1 b2)
  (setf (ssa-block-succ b1) (ssa-block-index b2)))

(defun insert-block-jump (block jump-block-index)
  (setf (ssa-block-jump block) jump-block-index))

(defun pop-labels-env (lambda-ssa)
  (let ((ssa-env (lambda-ssa-env lambda-ssa)))
    (pop (ssa-env-labels ssa-env))))

(defun push-labels-env (lambda-ssa)
  (let ((ssa-env (lambda-ssa-env lambda-ssa)))
    (push (make-slabels) (ssa-env-labels ssa-env))))

(defun ssa-add-block-label (lambda-ssa block label)
  (let* ((env (lambda-ssa-env lambda-ssa))
	 (slabels (first (ssa-env-labels env))))
    (push (cons label (ssa-block-index block))
	  (slabels-alist slabels))))

(defun ssa-find-block-index-by-label (lambda-ssa label)
  (let* ((env (lambda-ssa-env lambda-ssa))
	 (slabels (ssa-env-labels env)))
    (dolist (slabel slabels)
      (let ((cons (assoc label (slabels-alist slabel))))
	(when cons
	  (return-from ssa-find-block-index-by-label (cdr cons) ))))))

(defparameter *ir-index-counter* 0)
(defun emit (lambda-ssa ssa block)
  (declare (ignore lambda-ssa)
	   (optimize (debug 3)))
  (when (ssa-form-p ssa)
    (setf (ssa-form-index ssa) *ir-index-counter*)
    (incf *ir-index-counter*))
  (push ssa (ssa-block-ir block)))

(defun emit-if-node-ssa (if-node lambda-ssa place block)
  (let* ((test-node (if-node-test-form if-node))
	 (test-place (generate-temp-place))
	 (false-block (make-new-ssa-block))
	 (true-block (make-new-ssa-block))
	 (next-block (make-new-ssa-block)))
    (ssa-connect-blocks true-block next-block)
    (ssa-connect-blocks false-block next-block)
    (ssa-connect-blocks block false-block)
    (ssa-add-block lambda-ssa false-block)
    (ssa-add-block lambda-ssa true-block)
    (ssa-add-block lambda-ssa next-block)
    (emit-ssa test-node lambda-ssa test-place block)
    (emit-ssa (if-node-true-form if-node) lambda-ssa place true-block)
    (emit-ssa (if-node-false-form if-node) lambda-ssa place false-block)
    (insert-block-jump block (ssa-block-index true-block))
    (emit lambda-ssa
	  (make-ssa-if
	   :test test-place
	   :true-block (ssa-block-index true-block))
	  block)
    next-block))

(defun maybe-emit-direct-load (node lambda-ssa place block)
  (etypecase node
    (immediate-constant-node
     (emit lambda-ssa (make-ssa-load :to place
    				     :from node) block)
     block)
    (lexical-var-node
     (emit lambda-ssa (make-ssa-load :to place
    				     :from (make-var-place :name (lexical-var-node-name node))) block)
     block)
    (t (emit-ssa node lambda-ssa place block ))))

;;; FIXME, fun can be CLOSURE or LAMBDA
(defun emit-call-node-ssa (node lambda-ssa place block)
  (let ((fun (call-node-function node))
	(arguments (call-node-arguments node))
	(args-places nil))
    (dolist (arg arguments)
      (let* ((place (generate-temp-place))
	     (new-block (maybe-emit-direct-load arg lambda-ssa place block)))
	(setf block new-block)
	(push place args-places)))
    (let ((arg-index 0))
      (dolist (p (reverse args-places))
	(emit lambda-ssa (make-ssa-load :to (make-argument-place :index arg-index) :from p) block)
	(incf arg-index)))
    (if (null place)
	(progn
	  (emit lambda-ssa
		(make-ssa-unknown-values-fun-call :fun fun)
		block)
	  block)
	(progn
	  (emit lambda-ssa (make-ssa-fun-call :fun fun) block)
	  (emit lambda-ssa (make-ssa-load :to place :from (make-return-value-place :index 0)) block)
	  block))))

(defun emit-lexical-binding-node-ssa (node lambda-ssa block)
  (let ((lvar (lexical-binding-node-name node))
	(form (lexical-binding-node-form node)))
    (maybe-emit-direct-load form lambda-ssa lvar block)))

(defun emit-let-node-ssa (node lambda-ssa place block)
  (dolist (n (let-node-bindings node))
    (let ((new-block (emit-lexical-binding-node-ssa n lambda-ssa block)))
      (setf block new-block)))
  (emit-ssa (let-node-form node) lambda-ssa place block))

(defun emit-immediate-node-ssa (node lambda-ssa place block)
  (if place
      (emit lambda-ssa (make-ssa-load :to place :from node) block)
      (emit lambda-ssa node block))
  block)

(defun emit-lexical-var-node (node lambda-ssa place block)
  (if place
      (emit lambda-ssa (make-ssa-load :to place :from (make-var-place :name (lexical-var-node-name node))) block )
      (emit lambda-ssa (make-var-place :name (lexical-var-node-name node)) block))
  block)

(defun emit-progn-node-ssa (node lambda-ssa place block)
  (do* ((forms (progn-node-forms node) (cdr forms))
	(form-node (car forms) (car forms)))
       ((null forms) block)
    (if (cdr forms)
	(setf block (emit-ssa form-node lambda-ssa nil block))
	(setf block (emit-ssa form-node lambda-ssa place block)))))

(defun emit-tagbody-node-ssa (node lambda-ssa place block)
  (push-labels-env lambda-ssa)
  (dolist (form-node (tagbody-node-forms node))
    (let ((new-block (emit-ssa form-node lambda-ssa nil block)))
      (setf block new-block)))
  (pop-labels-env lambda-ssa)
  (if place
      (emit lambda-ssa (make-ssa-load :to place :from (make-immediate-constant :constant *nil*)) block)
      (emit lambda-ssa (make-immediate-constant :constant *nil*) block))
  block)

(defun emit-tagbody-label-node-ssa (node lambda-ssa place block)
  (declare (ignore place))
  (let ((new-block (make-new-ssa-block)))
    (ssa-add-block lambda-ssa new-block)
    (ssa-connect-blocks block new-block)
    (ssa-add-block-label lambda-ssa new-block (label-node-label node))
    (emit lambda-ssa (make-ssa-label :label (label-node-label node)) new-block)
    new-block))

(defun emit-go-node-ssa (node lambda-ssa place block)
  (declare (ignore place))
  (let ((new-block (make-new-ssa-block))
	(label-name (label-node-label (go-node-label-node node)) ))
    (ssa-add-block lambda-ssa new-block)
    (insert-block-jump block (ssa-find-block-index-by-label lambda-ssa label-name))
    (emit lambda-ssa (make-ssa-go :label label-name) block)
    new-block))

(defun emit-setq-node-ssa (node lambda-ssa place block)
  (maybe-emit-direct-load (setq-node-form node) lambda-ssa (setq-node-var node) block )
  (when place
    (maybe-emit-direct-load (setq-node-var node)  lambda-ssa place block))
  block)

(defun emit-ssa (node lambda-ssa &optional place block)
  (etypecase node
    (if-node (emit-if-node-ssa node lambda-ssa place block))
    (call-node (emit-call-node-ssa node lambda-ssa place block))
    (let-node (emit-let-node-ssa node lambda-ssa place block))
    (progn-node (emit-progn-node-ssa node lambda-ssa place block))
    (lexical-var-node (emit-lexical-var-node node lambda-ssa place block))
    (immediate-constant-node (emit-immediate-node-ssa node lambda-ssa place block))
    (tagbody-node (emit-tagbody-node-ssa node lambda-ssa place block))
    (label-node (emit-tagbody-label-node-ssa node lambda-ssa place block))
    (go-node (emit-go-node-ssa node lambda-ssa place block))
    (setq-node (emit-setq-node-ssa node lambda-ssa place block))))

(defun ssa-normalize-lambda-ssa (lambda-ssa)
   (let ((blocks nil))
    (dolist (b (lambda-ssa-blocks lambda-ssa))
      (setf (ssa-block-ir b)
	    (reverse (ssa-block-ir b)))
      (push b blocks))
    (setf (lambda-ssa-blocks lambda-ssa) blocks))
  lambda-ssa)

(defun ssa-parse-lambda (lambda-node)
  (let* ((*ssa-block-counter* 0)
	 (*ir-index-counter* 0)
	 (lambda-ssa (make-lambda-ssa))
	 (entry-block (make-new-ssa-block)))
    (ssa-add-block lambda-ssa entry-block)
    (emit-ssa (lambda-node-body lambda-node) lambda-ssa nil entry-block)
    (ssa-normalize-lambda-ssa lambda-ssa)))
