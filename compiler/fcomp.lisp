(in-package #:clcomp)
(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defun eval-at-compile-timep (eval-conditions)
  (find :compile-toplevel eval-conditions))

(defun clcomp-compile-top-level-form (form)
  (if (atom form)
      form
      (let ((expanded (clcomp-macroexpand form)))
	(case (first expanded)
	  (eval-when (when (eval-at-compile-timep (second expanded))
		       (clcomp-eval (cons 'progn (cddr expanded)))))
	  (progn (dolist (f (cdr expanded))
		   (clcomp-compile-top-level-form f)))
	  (%defun (clcomp-compile (second expanded)
				  (third expanded)))
	  (%defparameter (clcomp-compile nil
					 (list 'lambda nil
					       form)
					 :eval-at-load t))
	  (otherwise (clcomp-compile nil
				     (list 'lambda nil
					   form)
				     :eval-at-load t))))))

(defun clcomp-compile-file (file)
  (with-open-file (s file)
    (loop for f = (read s nil)
	  while f
	  do
	     (clcomp-compile-top-level-form f))))
