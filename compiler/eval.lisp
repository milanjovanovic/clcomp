(in-package #:clcomp)


;; FIXME, implement eval as load time fixup eval
(defun %clcomp-eval (exp)
  (declare (optimize (speed 0) (debug 3)))
  (let ((f (first exp)))
    (case f
      (progn (dolist (f1 (cdr exp))
	       (%clcomp-eval f1)))
      (defsetf (let ((accessor (second exp))
		     (setter (third exp)))
		 (push (cons accessor setter) *defsetfs*)))
      (otherwise (apply f (rest exp))))))

(defun %compiler-defparameter (s)
  s)

(defun clcomp-eval (exp)
  (declare (optimize (debug 3)))
  (let ((macroexpanded (clcomp-macroexpand exp (create-macros-env t t))))
    ;; (reset-temp-location-counter)
    (%clcomp-eval macroexpanded)))

(defun clcomp-load (file)
  (with-open-file (f file)
    (loop for form = (read f nil nil)
	  while form
	  collect (clcomp-eval form))))
