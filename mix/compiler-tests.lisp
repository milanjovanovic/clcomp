(in-package :clcomp)

(defun dump-core (file form)
  (rt-reset)
  (clcomp-compile-file (format nil "~a/code/cons.lisp" *clcomp-home*))
  (clcomp-compile-file (format nil "~a/code/array.lisp" *clcomp-home*))
  (clcomp-compile-file (format nil "~a/code/arith.lisp" *clcomp-home*))
  (clcomp-compile-file (format nil "~a/code/call.lisp" *clcomp-home*))
  (set-start-address)
  (clcomp-compile nil form)
  (rt-dump-binary file))

(defun make-core-file-name (name result)
  (if result
      (format nil "~a/runtime/compiler-regression-tests/T/~a" *clcomp-home* name)
      (format nil "~a/runtime/compiler-regression-tests/NIL/~a" *clcomp-home* name)))


(defun simple-test-1 ()
  (dump-core (make-core-file-name "simple-test-1" t)
	     '(lambda ()
	       (= 0 0))))

(defun simple-test-2 ()
  (dump-core (make-core-file-name "simple-test-2" nil)
	     '(lambda ()
	       (= 1 0))))

(defun cons-test-1 ()
  (dump-core (make-core-file-name "list-test" t)
	     '(lambda ()
	       (let ((l (cons 1 (cons 2 (cons 3 nil)))))
		 (setf (car l) 10)
		 (let ((c (car l)))
		   (= c 10))))))

(defun cons-test-2 ()
  (dump-core (make-core-file-name "list-test-notinline" t)
	     '(lambda ()
	       (declare (notinline cons car))
	       (let ((l (cons 1 (cons 2 (cons 3 nil)))))
		 (setf (car l) 10)
		 (let ((c (car l)))
		   (= c 10))))))

(defun array-test-1 ()
  (dump-core (make-core-file-name "array-test-1" t)
	     '(lambda ()
	       (let ((a (make-array 3 (cons 1 (cons 2 (cons 3 nil))))))
		 (setf (aref a 1) 20)
		 (and (= (aref a 0) 1)
		      (= (aref a 1) 20)
		      (= (aref a 2) 3))))))

(defun array-test-2 ()
  (dump-core (make-core-file-name "array-test-2" nil)
	     '(lambda ()
	       (let ((a (make-array 3 (cons 1 (cons 2 (cons 3 nil))))))
		 (setf (aref a 1) 20)
		 (and (= (aref a 0) 1)
		      (= (aref a 1) 20)
		      (= (aref a 2) 300))))))


(defun generate-all-test-cores ()
  (let ((*debug* nil))
    (simple-test-1)
    (simple-test-2)
    (cons-test-1)
    (cons-test-2)
    (array-test-1)
    (array-test-2)
    (values)))
