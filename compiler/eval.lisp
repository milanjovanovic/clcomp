(in-package #:clcomp)

(defun %clcomp-eval-compile (exp)
  (let ((compile-component (clcomp-compile (third exp))))
    compile-component))

(defun %clcomp-compile-and-eval (exp)
  (let ((compile-component (clcomp-compile (list 'lambda nil exp))))
    (list 'rt-funcall compile-component)))

;; FIXME, for now it's all compile/eval/execute
(defun %clcomp-eval-when (when what)
  (declare (ignore when))
  (let ((r nil))
    (dolist (w what)
      (setf r (%clcomp-eval w)))
    r))

(defun %clcomp-eval (exp)
  (let ((f (first exp)))
    (cond ((eq f 'progn)
	   (let ((x nil))
	     (dolist (e (rest exp))
	       (setf x (%clcomp-eval e)))
	     x))
	  ((eq f 'eval-when)
	   (%clcomp-eval-when (second exp) (cddr exp)))
	  ((eq f '%rt-defun)
	   (rt-defun (second exp)))
	  ((eq f '%defun)
	   (%clcomp-eval-compile exp))
	  (t
	   (%clcomp-compile-and-eval exp)))))

(defun clcomp-eval (exp)
  (reset-temp-location-counter)
  (%clcomp-eval (expand exp)))

(defun clcomp-load (file)
  (with-open-file (f file)
    (loop for form = (read f nil nil)
	  while form
	  collect (clcomp-eval form))))
