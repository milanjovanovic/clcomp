(defun %get-env ()
  (declare (inline %get-env))
  (%get-env))

(defun %set-env (env)
  (declare (inline %set-env))
  (%set-env env))

(defun %get-global-symbols ()
  (car (%get-env)))

(defun %get-global-interned-symbol (symbol-name)
  (dolist (cons (%get-global-symbols))
    (let ((sname (car cons))
	  (symbol (cdr cons)))
      (when (string-equal symbol-name sname)
	(return-from %get-global-interned-symbol symbol)))))

(defun %add-to-interned-symbols (symbol)
  (setf (car (%get-env))
	(cons (cons (symbol-name symbol) symbol)
	      (car (%get-env)))))

(defun %initialize-env ()
  (%set-env (list nil nil))
  (%add-to-interned-symbols 'character)
  (%add-to-interned-symbols 'simple-array))

(defun %init-runtime ()
  (%initialize-env))
