(in-package #:clcomp)

(defun %eval-lambda (exp)
  (clcomp-compile exp))

(defun %eval-defun (exp)
  (let ((exp (expand exp)))
    (list 'setf 'symbol-function (second exp)
	  (%eval-lambda (third exp)))))

(defun %eval-in-lambda (exp)
  (list 'funcall
   (%eval-lambda (list 'lambda nil exp))))

(defun %clcomp-eval (exp)
  (if (atom exp)
      ;; FIXME
      (cond ((symbolp exp) (list 'symbol exp))
	    (t (list 'atom exp)))
      (let ((first (car exp)))
	(cond ((eq first 'quote) (second exp))
	      ((eq first 'lambda) (%eval-lambda exp))
	      ((eq first 'defun) (%eval-defun exp))
	      ;; FIXME
	      (t (%eval-in-lambda exp))))))

(defun clcomp-eval (exp)
  (reset-temp-location-counter)
  (reset-env-counter)
  (%clcomp-eval exp))

;;; for now there is no support for closures
