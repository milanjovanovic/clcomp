;; we need $DEFUN instead of DEFUN because at this point we still don't have ENV setup
;; side  effect is that we can't use FUNCALL on symbol for functions defined with $DEFUN
;; FIXME
($defun %get-env ()
  (declare (inline %get-env))
  (%get-env))

($defun %set-env (env)
  (declare (inline %set-env))
  (%set-env env))

($defun %find-package-symbols (list package)
  (dolist (cons list)
    (when (string-equal package (first cons))
      (return-from %find-package-symbols cons))))

($defun %get-global-interned-symbol (symbol-name package)
  (let* ((package-symbols (%find-package-symbols (car (%get-env)) package)))
    (when package-symbols
      (dolist (cons (second package-symbols))
	(let ((sname (car cons))
	      (symbol (cdr cons)))
	  (when (string-equal symbol-name sname)
	    (return-from %get-global-interned-symbol symbol)))))))

($defun %add-to-interned-symbols (symbol package)
  (let* ((symbols-env (car (%get-env)))
	 (package-symbols (%find-package-symbols symbols-env package)))
    (if package-symbols
	(setf (car (cdr package-symbols))
	      (cons (cons (symbol-name symbol) symbol)
		    (car (cdr package-symbols))))
	(setf (car (%get-env))
	      (cons (list package (list (cons (symbol-name symbol) symbol)))
		    symbols-env)))))

($defun %initialize-env ()
  (%set-env (list nil nil)))

;;; this needs to be evaluated first at load time 
(%initialize-env)


;; this is after %INIT-RUNTIME because it's creating fixup for string "CL"
;; and this can't be evaluated before env is created
($defun %intern-bootstrap-symbols ()
  (%add-to-interned-symbols 'simple-array "CL")
  (%add-to-interned-symbols 'character "CL")
  (%add-to-interned-symbols '*package* "CL")
  (%add-to-interned-symbols :element-type "KEYWORD")
  (%add-to-interned-symbols :initial-element "KEYWORD"))

(%intern-bootstrap-symbols)

(defparameter *package* "CL")
