(in-package #:clcomp)

(declaim (optimize (debug 3) (safety 3) (speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transform sexp expression to structures tree

(defstruct immediate-constant-node value)
(defstruct heap-constant-node form)
(defstruct quoted-node form)
(defstruct lexical-var-node name form rest) ; FIXME, make-fun-argument-node
(defstruct if-node test-form true-form false-form)
(defstruct let-node bindings form)
(defstruct progn-node forms)
(defstruct call-node function arguments)
(defstruct block-node name form)
(defstruct lambda-node name arguments declarations body)
(defstruct tagbody-node forms)
(defstruct go-node label-node)
(defstruct label-node label)
(defstruct setq-node var form)

(defun create-lambda-arguments-nodes (arguments)
  (let ((nodes nil)
	(rest-node nil))
    (dolist (argument arguments)
      (cond ((eq '&rest argument) (setf rest-node t))
	    (rest-node (push (make-lexical-var-node :name argument :form nil :rest t) nodes)
		       (setf rest-node nil))
	    (t (push (make-lexical-var-node :name argument :form nil) nodes))))
    (reverse nodes)))

(defun parse-declarations (form)
  (cdr form))

(defun create-lambda-node (form)
  (make-lambda-node :name nil
		    :declarations (parse-declarations (third form))
		    :arguments (create-lambda-arguments-nodes (second form))
		    :body (create-node (fourth form))))

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
  (cond ((eq form nil)
	 (make-immediate-constant-node :value *nil*))
	((eq form t)
	 (make-immediate-constant-node :value *t*))
	((integerp form)
	 (make-immediate-constant-node :value (fixnumize form)))
	((characterp form)
	 (make-immediate-constant-node :value (characterize form)))
	(t (make-heap-constant-node :form form))))

(defun create-tagbody-node (forms)
  (make-tagbody-node :forms (mapcar (lambda (exp)
				      (if (symbolp exp)
					  (make-label-node :label exp)
					  (create-node exp)))
				    (rest forms))))

(defun create-setq-node (form)
  (make-setq-node :var (make-lexical-var-node :name (second form)) :form (create-node (third form))))

(defun create-go-node (form)
  (make-go-node :label-node (make-label-node :label (second form))))

(defun create-node (form)
  (if (atom form)
      (cond ((constantp form)
	     (create-constant-node form))
	    ((make-lexical-var-node :name form :form nil)))
      (let ((first (first form)))
	(cond ((eq first '%compile-constant) ;; FIXME, need this for now, just for testing
	       (make-immediate-constant-node :value (second form)))
	      ((eq first 'quote)
	       (make-quoted-node :form (second form)))
	      ((eq first 'lambda)
	       (create-lambda-node form))
	      ((eq first 'if)
	       (create-if-node form))
	      ;; FIXME, currently let behave as let*
	      ((or (eq first 'let)
		   (eq first 'let*))
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

;;; TODO
;; RETURN
;; check syntax
;; check for t nil as arguments
