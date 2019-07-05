(in-package :clcomp)

(defparameter *macros* (make-hash-table))
(defparameter *setf-expanders* (make-hash-table))

(defun macro-dotimes (form)
  (let ((limit (gensym "LIMIT-"))
	(tag1 (gensym "TAG1-"))
	(vsym (first (second form))))
    (list 'block nil
	  (list 'let (list (list vsym 0)
			   (list limit (second (second form))))
		(list 'tagbody tag1
		      (list 'progn
			    (cons 'progn (cddr form))
			    (list 'setf vsym (list '+ 1 vsym))
			    (list 'if (list '>= vsym limit)
				  (list 'return-from nil nil)
				  (list 'go tag1))))))))
(setf (gethash 'dotimes *macros*) 'macro-dotimes)


(defun macro-dolist (form)
  (let ((olist (gensym "LIST-"))
	(list-cdr (gensym "LIST-CDR"))
	(tag1 (gensym "TAG1-"))
	(vsym (first (second form))))
    (list 'block nil
	  (list 'let* (list (list olist (second (second form)))
			    (list list-cdr olist)
			    (list vsym (list 'car list-cdr)))
		(list 'tagbody tag1
		      (list 'progn
			    (list 'if (list 'null vsym)
				  (list 'return-from nil nil))
			    (cons 'progn (cddr form))
			    (list 'setf list-cdr (list 'cdr list-cdr))
			    (list 'setf vsym (list 'car list-cdr))
			    (list 'go tag1)))))))
(setf (gethash 'dolist *macros*) 'macro-dolist)


(defun macro-return (form)
  (list 'return-from nil (second form)))
(setf (gethash 'return *macros*) 'macro-return)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct macros-env blocks)

(defun get-block-or-return-from-symbol (env block-symbol base-name)
  (let* ((hash-key (concatenate 'string (symbol-name block-symbol) base-name))
	 (sym (gethash hash-key (env-blocks env))))
    (if sym
	sym
	(let ((sym (gensym base-name)))
	  (setf (gethash hash-key (env-blocks env)) sym)
	  sym))))

(defun block-to-value-name (env block-name)
  (get-block-or-return-from-symbol env block-name "BLOCK-LET-VAR-"))

(defun block-to-label-name (env block-name)
  (get-block-or-return-from-symbol env block-name "BLOCK-TAGBODY-"))

(defun %clcomp-macroexpand-let-bindings (bindings env)
  (mapcar (lambda (b)
	    (list (first b) (clcomp-macroexpand (second b) env)))
	  bindings))

(defun %clcomp-macroexpand-let (form env)
  (list (first form) (%clcomp-macroexpand-let-bindings (second form) env)
	(cons 'progn (mapcar (lambda (f)
			       (clcomp-macroexpand f env))
			     (cddr form)))))

(defun %clcomp-macroexpand-block (form env)
  (let ((tagbody-symbol (block-to-label-name env (second form)))
	(ret-value-symbol (block-to-value-name env (second form))))
    (clcomp-macroexpand
     (list 'let (list (list ret-value-symbol nil))
	   (list 'progn
		 (list 'tagbody
		       (list 'setq ret-value-symbol
			     (append (list 'progn) (nthcdr 2 form)))
		       tagbody-symbol)
		 ret-value-symbol))
     env)))

(defun %clcomp-macroexpand-return-from (form env)
  (let ((tagbody-symbol (block-to-label-name env (second form)))
	(ret-value-symbol (block-to-value-name env (second form))))
    (clcomp-macroexpand
     (list 'progn
	   (list 'setq ret-value-symbol (third form))
	   (list 'go tagbody-symbol))
     env)))

(defun %clcomp-macroexpand-all (form env)
  (mapcar (lambda (f) (clcomp-macroexpand f env))
	  form))

(defun %clcomp-macroexpand-progn (form env)
  (cons 'progn
	(mapcar (lambda (f)
		       (clcomp-macroexpand f env))
		(cdr form))))

(defun %clcomp-macroexpand (macro-form env)
  (let* ((macro-fun (gethash (first macro-form) *macros*))
	 (expanded (funcall macro-fun macro-form)))
    (if (consp expanded)
	(clcomp-macroexpand expanded env)
	expanded)))

;; FIXME, %rt-defun ?!?!
(defun %clcomp-macroexpand-defun (form env)
  (list 'progn (list 'eval-when '(:compile-toplevel) (list '%rt-defun (second form)))
	(list '%defun (second form)
	      (clcomp-macroexpand (list 'lambda (third form) (fourth form)) env))))

(defun %clcomp-macroexpand-eval-when (form env)
  (list 'eval-when (second form)
	(clcomp-macroexpand (cons 'progn (cddr form)) env)))

(defun %clcomp-macroexpand-lambda (form env)
  (list 'lambda (second form)
	(clcomp-macroexpand (cons 'progn (cddr form)) env)))

;; FIXME, implement setf macro expanders
(defun %clcomp-macroexpand-setf (form env)
  (let ((where (second form))
	(what (third form)))
    (if (symbolp where)
	(list 'setq where (clcomp-macroexpand what env))
	(let* ((accessor (first where)))
	  (cond
	    ((eq accessor 'car)
	     (list '%setf-car (clcomp-macroexpand (second where) env) (clcomp-macroexpand what env)))
	    ((eq accessor 'cdr)
	     (list '%setf-cdr (clcomp-macroexpand (second where) env) (clcomp-macroexpand what env))))))))

(defun clcomp-macroexpand (form &optional env)
  (unless env
    (setf env (make-macros-env :blocks (make-hash-table :test 'equalp))))
  (if (atom form)
      form
      (let ((first (first form)))
	(let ((macro-fun (gethash first *macros*)))
	  (if macro-fun
	      (%clcomp-macroexpand form env)
	      (case first
		;; FIXME, transform let* to let
		((let let*) (%clcomp-macroexpand-let form env))
		(block (%clcomp-macroexpand-block form env))
		(return-from (%clcomp-macroexpand-return-from form env))
		(progn (%clcomp-macroexpand-progn form env))
		(defun (%clcomp-macroexpand-defun form env))
		(eval-when (%clcomp-macroexpand-eval-when form env))
		(lambda (%clcomp-macroexpand-lambda form env))
		(setf (%clcomp-macroexpand-setf form env))
		(otherwise (%clcomp-macroexpand-all form env))))))))






