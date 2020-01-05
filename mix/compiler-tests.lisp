(in-package :clcomp)

(defun dump-core (file form)
  (rt-reset)
  (clcomp-compile-file (format nil "~a/code/base.lisp" *clcomp-home*))
  (clcomp-compile-file (format nil "~a/code/global.lisp" *clcomp-home*))
  (clcomp-compile-file (format nil "~a/code/cons.lisp" *clcomp-home*))
  (clcomp-compile-file (format nil "~a/code/array.lisp" *clcomp-home*))
  (clcomp-compile-file (format nil "~a/code/seq.lisp" *clcomp-home*))
  (clcomp-compile-file (format nil "~a/code/arith.lisp" *clcomp-home*))
  (clcomp-compile-file (format nil "~a/code/call.lisp" *clcomp-home*))
  (clcomp-compile-file (format nil "~a/code/symbol.lisp" *clcomp-home*))
  (clcomp-compile-file (format nil "~a/code/char.lisp" *clcomp-home*))
  (set-start-address)
  (clcomp-compile nil form)
  (rt-dump-binary file))

(defun make-core-file-name (name result)
  (if result
      (format nil "~a/runtime/compiler-regression-tests/T/~a" *clcomp-home* name)
      (format nil "~a/runtime/compiler-regression-tests/NIL/~a" *clcomp-home* name)))

(defparameter *compiler-tests* (make-hash-table :test 'equalp))

(defmacro define-compiler-test (name result lambda)
  `(setf (gethash ,name *compiler-tests*)
	 (lambda ()
	   (dump-core (make-core-file-name ,name ,result) ',lambda))))


(define-compiler-test "simple-test-1" t (lambda ()
					  (= 0 0)))

(define-compiler-test "simple-test-2" nil (lambda ()
					    (= 1 0)))


;;; CONS/LIST
(define-compiler-test "cons-test-1" t (lambda ()
					(let ((l (cons 1 (cons 2 (cons 3 nil)))))
					  (setf (car l) 10)
					  (let ((c (car l)))
					    (= c 10)))))

(define-compiler-test "cons-test-1-inline" t (lambda ()
					       (declare (inline cons car))
					       (let ((l (cons 1 (cons 2 (cons 3 nil)))))
						 (setf (car l) 10)
						 (let ((c (car l)))
						   (= c 10)))))

(define-compiler-test "list-test-1" t (lambda ()
					(let ((l (list 1 2 3 4 5 6)))
					  (= 6 (+ (car l) 5)))))

(define-compiler-test "list-test-1-inline" t (lambda ()
					       (declare (inline car))
					       (let ((l (list 1 2 3 4 5 6)))
						 (= 6 (+ (car l) 5)))))

(define-compiler-test "list-test-2" t (lambda ()
					(let ((l (list 1 2 3 4 5 6)))
					  (= 6 (list-length l)))))

(define-compiler-test "list-test-3" t (lambda ()
					(let ((l (list 1 2 3 4 5 6)))
					  (= 4 (list-length (cddr l))))))

(define-compiler-test "list-test-4" nil (lambda ()
					  (let ((l (list 1 2 3 4 5 6)))
					    (= 4 (list-length l)))))
(define-compiler-test "list-test-5" t (lambda ()
					(let ((l (list 1 2 3 4 5 6)))
					  (and
					   (= 6 (car (list-reverse l)))
					   (= 6 (list-length (list-reverse l)))))))

(define-compiler-test "list-test-6" t (lambda ()
					(= 3 (length (list 1 2 3)))))

(define-compiler-test "list-append-1" t (lambda ()
					  (= 6 (length (append (list 1 2)
							       (list 3 4)
							       (list 5 6))))))

(define-compiler-test "list-test-7" t (lambda ()
					(= 3 (cddr (list* 1 2 3)))))


;;; ARRAY
(define-compiler-test "array-test-1"t (lambda ()
					(let ((a (make-array 3 (cons 1 (cons 2 (cons 3 nil))))))
					  (setf (aref a 1) 20)
					  (and (= (aref a 0) 1)
					       (= (aref a 1) 20)
					       (= (aref a 2) 3)))))

(define-compiler-test "array-test-2" nil (lambda ()
					   (let ((a (make-array 3 (cons 1 (cons 2 (cons 3 nil))))))
					     (setf (aref a 1) 20)
					     (and (= (aref a 0) 1)
						  (= (aref a 1) 20)
						  (= (aref a 2) 300)))))

;;; STRINGe
(define-compiler-test "string-test-1" t (lambda ()
					  (let* ((s1 "test string")
						 (s2 (%copy-array s1))
						 (s3 (string-upcase s2)))
					    (and (= (length s2) 11)
						 (char= (char s3 0) #\T)
						 (char= (char s3 1) #\E)
						 (char-equal (char s3 0) (char s2 0) (char s1 0))))))




(defun generate-all-test-cores ()
  (let ((*debug* nil))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (funcall v))
	     *compiler-tests*)))


