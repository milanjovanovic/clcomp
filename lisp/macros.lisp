(in-package :clcomp)

(defparameter *macros* (make-hash-table))

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

(defstruct env)

(defun %clcomp-macroexpand-let-bindings (bindings)
  (mapcar (lambda (b)
	    (list (first b) (clcomp-macroexpand (second b))))
	  bindings))

(defun %clcomp-macroexpand-let (form)
  (cons (list 'let (%clcomp-macroexpand-let-bindings (second form)))
	(mapcar #'clcomp-macroexpand (cddr form))))

;;; FIXME
(defun %clcomp-macroexpand-block (form)
  (cons (list 'block (second form))
	(mapcar #'clcomp-macroexpand (cddr form))))

(defun %clcomp-macroexpand-return-from (form)
  (list 'return-from * (second form) (clcomp-macroexpand (third form))))

(defun %clcomp-macroexpand-fun-call (form)
  (cons (first form)
	(cons (second form)
	      (mapcar #'clcomp-macroexpand (cddr form)))))

(defun %clcomp-macroexpand-progn (form)
  (cons 'progn
	(mapcar #'clcomp-macroexpand (cdr form))))

(defun %clcomp-macroexpand (macro-form)
  (let* ((macro-fun (gethash (first macro-form) *macros*))
	 (expanded (funcall macro-fun macro-form)))
    (if (consp expanded)
	(mapcar #'clcomp-macroexpand expanded)
	expanded)))

(defun clcomp-macroexpand (form &optional env)
  (if (atom form)
      form
      (let ((first (first form)))
	(let ((macro-fun (gethash first *macros*)))
	  (if macro-fun
	      (%clcomp-macroexpand form)
	      (case first
		(let (%clcomp-macroexpand-let form))
		(block (%clcomp-macroexpand-block form))
		(return (%clcomp-macroexpand-return-from form))
		(progn (%clcomp-macroexpand-progn form))
		(defun form)
		(lambda form)
		(setf form)
		(otherwise (%clcomp-macroexpand-fun-call form))))))))




