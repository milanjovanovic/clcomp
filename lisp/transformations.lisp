(in-package #:clcomp)

(declaim (optimize (debug 3) (safety 3) (speed 0)))


(defparameter *expand-symbols* nil)

(defun get-symbol-from-cache (base-symbol new-name)
  (let* ((hash-key (concatenate 'string (symbol-name base-symbol) new-name))
	 (sym (gethash hash-key *expand-symbols*)))
    (if sym
	sym
	(let ((sym (gensym new-name)))
	  (setf (gethash hash-key *expand-symbols*) sym)
	  sym))))

(defun create-block-tagbody-symbol (block-name)
  (get-symbol-from-cache block-name "BLOCK-TAGBODY-"))

(defun create-block-let-value-symbol (block-name)
  (get-symbol-from-cache block-name "BLOCK-LET-VAR-"))

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
  (list 'progn (list 'eval-when '(:compile-toplevel) (list '%rt-defun (second defun-form)))
	(list '%defun (second defun-form)
	      (%expand (list 'lambda (third defun-form) (fourth defun-form))))))

(defun transform-block (form)
  (let ((tagbody-symbol (create-block-tagbody-symbol (second form)))
	(ret-value-symbol (create-block-let-value-symbol (second form))))
    (list 'let (list (list ret-value-symbol nil))
	  (list 'progn
		(list 'tagbody
		      (list 'setq ret-value-symbol
			    (append (list 'progn) (mapcar #'%expand (nthcdr 2 form))))
		      tagbody-symbol)
		ret-value-symbol))))

(defun transform-return-from (form)
  (let ((tagbody-symbol (create-block-tagbody-symbol (second form)))
	(ret-value-symbol (create-block-let-value-symbol (second form))))
    (list 'progn
	  (list 'setq ret-value-symbol (third form))
	  (list 'go tagbody-symbol))))

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
	  ((eq first 'block) (transform-block form))
	  ((eq first 'return-from) (transform-return-from form))
	  ((eq first 'return) (transform-return-from
			       (append (list 'return-from 'nil) (cdr form))))
	  (t (append (list first) (mapcar #'%expand rest)))))))

(defun expand (form)
  (let ((*expand-symbols* (make-hash-table :test 'equalp)))
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
(defstruct go-node label-node)
(defstruct label-node label)
(defstruct setq-node var form)

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
  (cond ((eq form nil)
	 (make-immediate-constant-node :value *nil*))
	((eq form t)
	 (make-immediate-constant-node :value *t*))
	((integerp form)
	 (make-immediate-constant-node :value (fixnumize form)))
	((characterp form)
	 (make-immediate-constant-node :value form))
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

;;; TODO
;; RETURN
;; check syntax
;; check for t nil as arguments
