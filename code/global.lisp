(defun %get-env ()
  (declare (inline %get-env))
  (%get-env))

(defun %set-env (env)
  (declare (inline %set-env))
  (%set-env env))

(defun %get-global-symbols ()
  (aref (%get-env) 0))

(defun %get-global-interned-symbol (symbol-name)
  (dolist (cons (%get-global-symbols))
    (let ((sname (car cons))
	  (symbol (cdr cons)))
      (when (string-equal symbol-name sname)
	(return-from %get-global-interned-symbol symbol)))))

(defun %add-to-interned-symbols (symbol)
  (setf (aref (%get-env) 0)
	(cons (cons (symbol-name symbol) symbol)
	      (aref (%get-env) 0))))

(defun %initialize-env ()
  (let ((env (make-array 1 nil)))
    (setf (aref env 0) nil)
    (%set-env env)))
