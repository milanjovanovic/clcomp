(in-package #:clcomp)

(declaim (optimize (debug 3) (safety 3) (speed 0)))


(defun transform-lambda-form (lambda-form)
  (list 'lambda (second lambda-form)
	(append (list 'progn) (mapcar #'%expand (nthcdr 2 lambda-form)))))

(defun transform-let-bindings (bindings)
  (mapcar (lambda (pair)
	    (if (atom pair)
		(list pair nil)
		(list (first pair)
		      (%expand (second pair)))))
	  bindings))

(defun transform-let-form (let-form)
  (list 'let (transform-let-bindings (second let-form))
	(append (list 'progn) (mapcar #'%expand (nthcdr 2 let-form)))))

(defun transform-defun (defun-form)
  (list 'setf-symbol-function (second defun-form)
	(list 'lambda
	      (third defun-form)
	      (append (list 'progn) (mapcar #'%expand (nthcdr 3 defun-form))))))

(defun transform-setf-form (setf-form)
  (let ((place (second setf-form))
	(form (third setf-form)))
    (if (symbolp place)
	(list 'setq place (%expand form))
	(let ((accessor (first place)))
	  ;; FIXME, missing most of accessors
	  (cond
	    ((eq accessor 'car)
	     (list '%setf-car (%expand (second place)) (%expand form)))
	    ((eq accessor 'cdr)
	     (list '%setf-cdr (%expand (second place)) (%expand form))))))))

(defun %expand (form)
  (if (atom form)
      form
      (let ((first (first form))
	    (rest (rest form)))
	(cond
	  ((eq first 'defun) (transform-defun form))
	  ((eq first 'lambda) (transform-lambda-form form))
	  ((eq first 'let) (transform-let-form form))
	  ((eq first 'setf) (transform-setf-form form))	  
	  (t (append (list first) (mapcar #'%expand rest)))))))

(defun expand (form)
  (list 'lambda nil
	(%expand form)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transform sexp expression to structures tree

(defstruct immediate-constant-node value)
(defstruct heap-constant-node form)
(defstruct quoted-node form)
(defstruct lexical-var-node name form)
(defstruct if-node test-form true-form false-form)
(defstruct let-node bindings form)
(defstruct progn-node forms)
(defstruct call-node function arguments)
(defstruct block-node name form)
(defstruct lambda-node name arguments body)
(defstruct tagbody-node forms)
(defstruct setq-node symbol form)
(defstruct go-node tag)

(defun create-lambda-arguments-nodes (arguments)
  (mapcar (lambda (var)
	    (make-lexical-var-node :name var :form nil))
	  arguments))

(defun create-lambda-node (form)
  (make-lambda-node :name nil
		    :arguments (create-lambda-arguments-nodes (second form))
		    :body (create-node (third form))))

(defun create-if-node (form)
  (make-if-node :test-form (create-node (second form))
		:true-form (create-node (third form))
		:false-form (create-node (fourth form))))

(defun create-let-binding-nodes (bindings)
  (mapcar (lambda (binding)
	    (if (atom binding)
		(make-lexical-var-node :name binding :form nil)
		(make-lexical-var-node :name (first binding) :form (create-node (second binding)))))
	  bindings))

(defun create-let-node (form)
  (make-let-node :bindings (create-let-binding-nodes (second form))
		 :form (create-node (third form))))

(defun create-progn-node (form)
  (make-progn-node :forms (mapcar #'create-node (rest form))))

(defun create-call-node (form)
  (make-call-node :function (first form) :arguments (mapcar #'create-node (rest form))))

(defun create-constant-node (form)
  (if (or (characterp form)
	  (integerp form))
      (make-immediate-constant-node :value form)
      (make-heap-constant-node :form form)))

(defun create-tagbody-node (forms)
  (make-tagbody-node :forms (mapcar #'create-node (rest forms))))

(defun create-setq-node (form)
  (make-setq-node :symbol (second form) :form (create-node (third form))))

(defun create-go-node (form)
  (make-go-node :tag (create-node (second form))))

(defun create-node (form)
  (if (atom form)
      (cond ((constantp form)
	     (create-constant-node form))
	    ((make-lexical-var-node :name form :form nil)))
      (let ((first (first form)))
	(cond ((eq first 'quote)
	       (make-quoted-node :form (second form)))
	      ((eq first 'lambda)
	       (create-lambda-node form))
	      ((eq first 'if)
	       (create-if-node form))
	      ((eq first 'let)
	       (create-let-node form))
	      ((eq first 'progn)
	       (create-progn-node form))
	      ((eq first 'tagbody)
	       (create-tagbody-node form))
	      ((eq first 'setq)
	       (create-setq-node form))
	      ((eq first 'go)
	       (create-go-node form))
	      (t (create-call-node form))))))
