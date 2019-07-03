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


(defun clcomp-macroexpand (form)
  (if (atom form)
      form
      (let ((maybe-macro (first form)))
	(let ((macro-fun (gethash maybe-macro *macros*)))
	  (if macro-fun
	      (mapcar #'clcomp-macroexpand (funcall macro-fun form))
	      (mapcar #'clcomp-macroexpand form))))))
