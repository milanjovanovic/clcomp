(in-package #:clcomp)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

#|
(defparameter *binary-functions* '((+ two-arg-+) (- two-arg--) (* two-arg-*) (/ two-arg-/)))

(defun is-binary-function (symbol)
  (assoc symbol *binary-functions*))

(defun get-two-arg-version (fun)
  (second (is-binary-function fun)))

;;; FIXME, wrong expanding of two-arg-funs
(defun transform-binary-function-form (form)
  (let* ((function (first form))
	 (two-arg-version (get-two-arg-version function))
	 (arguments (rest form))
	 (arg-size (length arguments)))
    (cond ((= arg-size 1) (expand-form (first arguments)))
	  ((= arg-size 2) (list two-arg-version
				(expand-form (first arguments))
				(expand-form (second arguments))))
	  (t
	   (transform-binary-function-form 
	    (append (list two-arg-version
			  (list two-arg-version
				(list two-arg-version (first arguments) (second arguments))
				(third arguments)))
		    (nthcdr 3 arguments)))))))
|#

(defun transform-lambda-form (lambda-form)
  (list 'lambda (second lambda-form)
	(append (list 'progn) (mapcar #'expand-form (nthcdr 2 lambda-form)))))

(defun transform-let-bindings (bindings)
  (mapcar (lambda (pair)
	    (if (atom pair)
		(list pair nil)
		(list (first pair)
		      (expand-form (second pair)))))
	  bindings))

(defun transform-let-form (let-form)
  (list 'let (transform-let-bindings (second let-form))
	(append (list 'progn) (mapcar #'expand-form (nthcdr 2 let-form)))))

(defun expand-form (form)
  (if (atom form)
      form
      (let ((first (first form))
	    (rest (rest form)))
	(cond
	  ((eq first 'lambda) (transform-lambda-form form))
	  ((eq first 'let) (transform-let-form form))
	  (t (append (list first) (mapcar #'expand-form rest)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transform sexp expression to structures tree

(defstruct constant-node value)
(defstruct quoted-node form)
(defstruct lexical-var-node name form)
(defstruct if-node test-form true-form false-form)
(defstruct let-node bindings form)
(defstruct progn-node forms)
(defstruct call-node function arguments)
(defstruct block-node name form)

(defstruct lambda-node name arguments body)

(defun create-lambda-arguments-nodes (arguments)
  (mapcar (lambda (var)
	    (make-lexical-var-node :name var :form nil))
	  arguments))

(defun create-lambda-node (form)
  (make-lambda-node :name nil
		    :arguments (create-lambda-arguments-nodes (second form))
		    :body (transform-to-nodes (third form))))

(defun create-if-node (form)
  (make-if-node :test-form (transform-to-nodes (second form))
		:true-form (transform-to-nodes (third form))
		:false-form (transform-to-nodes (fourth form))))

(defun create-let-binding-nodes (bindings)
  (mapcar (lambda (binding)
	    (if (atom binding)
		(make-lexical-var-node :name binding :form nil)
		(make-lexical-var-node :name (first binding) :form (transform-to-nodes (second binding)))))
	  bindings))

(defun create-let-node (form)
  (make-let-node :bindings (create-let-binding-nodes (second form))
		 :form (transform-to-nodes (third form))))

(defun create-progn-node (form)
  (make-progn-node :forms (mapcar #'transform-to-nodes (rest form))))

(defun create-call-node (form)
  (make-call-node :function (first form) :arguments (mapcar #'transform-to-nodes (rest form))))


(defun transform-to-nodes (form)
  (if (atom form)
      ;; FIXME, constants are not ok
      (cond ((constantp form)
	     (make-constant-node :value form))
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
	      (t (create-call-node form))))))

