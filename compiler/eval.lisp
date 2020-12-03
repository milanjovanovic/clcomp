(in-package #:clcomp)

(defun %compiler-load-time-eval (exp)
  (let ((f (first exp)))
    (case f
      (%%define-struct (clcomp-compile nil (list 'lambda nil
						 (list '%%define-struct
						       (list 'quote (second exp))
						       (list 'quote (third exp))
						       (list 'quote (fourth exp))))
				       :eval-at-load t))
      ;; (%%compiler-defun	(clcomp-compile nil (list 'lambda nil
      ;; 						  (list '%set-symbol-function
      ;; 							(list 'quote (second exp))
      ;; 							(list '%function (second exp))))
      ;; 					:eval-at-load t))
      )))


;; FIXME, implement eval as load time fixup eval
(defun %clcomp-eval (exp)
  (declare (optimize (speed 0) (debug 3)))
  (let ((f (first exp)))
    (case f
      (progn (dolist (f1 (cdr exp))
	       (%clcomp-eval f1)))
      (defsetf (apply '%%defsetf (cdr exp)))
      (%%define-struct (progn
			 (funcall '%%define-struct (second exp)
				  (third exp)
				  (fourth exp))
			 (%compiler-load-time-eval exp)))
      (%%compiler-defun (progn
			  (apply '%%compiler-defun (rest exp))
			  (%compiler-load-time-eval exp)))
      (%%compiler-defparameter (apply '%%compiler-defparameter (list (second (second exp)))))
      (otherwise (error "Unknown !")))))

(defun clcomp-eval (exp)
  (declare (optimize (debug 3)))
  (let ((macroexpanded (clcomp-macroexpand exp)))
    ;; (reset-temp-location-counter)
    (%clcomp-eval macroexpanded)))

(defun clcomp-load (file)
  (with-open-file (f file)
    (loop for form = (read f nil nil)
	  while form
	  collect (clcomp-eval form))))
