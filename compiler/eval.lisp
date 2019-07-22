(in-package #:clcomp)

(defun %clcomp-eval (exp)
  (let ((f (first exp)))
    (case f
      (progn (dolist (f1 (cdr exp))
	       (%clcomp-eval f1)))
      (otherwise (apply f (rest exp))))))

(defun clcomp-eval (exp)
  
  ;; (reset-temp-location-counter)
  (%clcomp-eval (clcomp-macroexpand exp)))

(defun clcomp-load (file)
  (with-open-file (f file)
    (loop for form = (read f nil nil)
	  while form
	  collect (clcomp-eval form))))
