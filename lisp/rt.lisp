(in-package #:clcomp)

(defparameter *runtime-heap-start* #x200000010)
(defparameter *rt-funs* (make-hash-table))


(defun rt-defun (name)
  ;; code loading will incfrease heap pointer
  (setf (gethash name *rt-funs*) *runtime-heap-start*))

(defun get-rt-defun (name)
  (gethash name *rt-funs*))
