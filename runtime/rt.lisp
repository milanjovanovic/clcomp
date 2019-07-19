(in-package #:clcomp)

(defparameter *runtime-heap-start* #x200000010)
(defparameter *compilation-start-addres* *runtime-heap-start*)
(defparameter *rt-funs* (make-hash-table))


(defstruct compilation units)
(defparameter *current-compilation* (make-compilation :units nil))


(defun rt-define-function (name compilation-unit)
  ;; code loading will increase heap pointer
  (setf (gethash name *rt-funs*) *compilation-start-addres*)
  (setf *compilation-start-addres* (+ *compilation-start-addres* (get-compilation-unit-code-size compilation-unit))))

(defun resolve-fixup (fixup unit)
  (declare (ignore fixup unit))
  "FIXME")

(defun resolve-fixups ()
  (dolist (unit (compilation-units *current-compilation*))
    (dolist (fixups (compilation-unit-fixups unit))
      (dolist (fixup fixups)
	(resolve-fixup fixup unit)))))

(defun write-binary (stream)
  (dolist (unit (compilation-units compilation))
    (let ((code (compilation-unit-code unit)))
      (dolist (inst code)
	(dolist (byte inst)
	  (write-byte byte f))))))

(defun rt-dump-binary (file)
  (resolve-fixups)
  (with-open-file (f file :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (dolist (c '(0 0 0 2 0 0 0 16))
      (write-byte c f))
    (let ((code (compilation-unit-code compilation-unit)))
      (dolist (inst code)
	(dolist (byte inst)
	  (write-byte byte f))))))

(defun rt-add-to-compilation (compilation-unit)
  (push (compilation-units *current-compilation*) compilation-unit))

(defun rt-get-fun-address (name)
  (gethash name *rt-funs*))

(defun clcomp-compile-file (file)
  "FIXME")

(defun compile-lib ()
  (let ((files '("arith.lisp" "cons.lisp"))
	(comps nil))
    (dolist (f files)
      (with-open-file (s (format nil "/Users/milan/projects/clcomp.github/lisp/lib/~a" f))
	(loop for exp = (read s nil)
	      while exp
	      do (push (clcomp-compile exp) comps))))))
