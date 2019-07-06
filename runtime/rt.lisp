(in-package #:clcomp)

(defparameter *runtime-heap-start* #x200000010)
(defparameter *rt-funs* (make-hash-table))


(defun rt-defun (name)
  ;; code loading will incfrease heap pointer
  (setf (gethash name *rt-funs*) *runtime-heap-start*))

(defun get-rt-defun (name)
  (gethash name *rt-funs*))

(defun compile-lib ()
  (let ((files '("arith.lisp" "cons.lisp"))
	(comps nil))
    (dolist (f files)
      (with-open-file (s (format nil "/Users/milan/projects/clcomp.github/lisp/lib/~a" f))
	(loop for exp = (read s nil)
	      while exp
	      do (push (clcomp-compile exp) comps))))))
