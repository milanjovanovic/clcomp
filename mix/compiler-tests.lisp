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
  (clcomp-compile-file (format nil "~a/code/tests.lisp" *clcomp-home*))
  (set-start-address (clcomp-compile nil form))
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
(define-compiler-test "array-test-1" t (lambda ()
					(let ((a (make-array 3 t nil  (cons 1 (cons 2 (cons 3 nil))))))
					  (setf (aref a 1) 20)
					  (and (= (aref a 0) 1)
					       (= (aref a 1) 20)
					       (= (aref a 2) 3)))))

(define-compiler-test "array-test-2" nil (lambda ()
					   (let ((a (make-array 3 t nil (cons 1 (cons 2 (cons 3 nil))))))
					     (setf (aref a 1) 20)
					     (and (= (aref a 0) 1)
						  (= (aref a 1) 20)
						  (= (aref a 2) 300)))))


(define-compiler-test "array-test-3" t (lambda ()
					   (let* ((a (make-array 3 'fixnum 0 nil))
						 (atype (type-of a)))
					     (and (= (aref a 0) 0)
						  (= (aref a 1) 0)
						  (= (aref a 2) 0)
						  (and (eq 'simple-array (first atype))
						       (eq 'fixnum (second atype))
						       (= 3 (first (third atype))))))))

;;; STRING
(define-compiler-test "string-test-1" t (lambda ()
					  (let* ((s1 "test string")
						 (s2 (%copy-array s1))
						 (s3 (string-upcase s2)))
					    (and (= (length s2) 11)
						 (char= (char s3 0) #\T)
						 (char= (char s3 1) #\E)
						 (char-equal (char s3 0) (char s2 0) (char s1 0))))))


;;; MIX
(define-compiler-test "mix-1" t (lambda ()
				  (let ((symbol 'bla))
				    (and (eq 'foo 'foo)
					 (not (eq 'foo 'bar))
					 (eq 'bla symbol)
					 (eq 'fixnum (type-of 10))))))


(define-compiler-test "mix-2" t (lambda ()
				  (and
				   (equal (rest-fixed-0) (list nil))
				   (equal (rest-fixed-0 1 2) (list (list 1 2)))
				   (equal (rest-fixed-0 1 2 3 4 5 6) (list (list 1 2 3 4 5 6)))
				   (equal (rest-fixed-4 1 2 3 4) (list 1 2 3 4 nil))
				   (equal (rest-fixed-4 1 2 3 4 5 6) (list 1 2 3 4 (list 5 6)))
				   (equal (rest-fixed-5 1 2 3 4 5) (list 1 2 3 4 5 nil))
				   (equal (rest-fixed-5 1 2 3 4 5 6 7 8) (list 1 2 3 4 5 (list 6 7 8)))
				   (equal (rest-fixed-6 1 2 3 4 5 6 7 8) (list 1 2 3 4 5 6 (list 7 8)))
				   (equal (rest-fixed-7 1 2 3 4 5 6 7 8) (list 1 2 3 4 5 6 7 (list 8))))))


(define-compiler-test "key-1" t (lambda ()
				  (and
				   (equal (key-fixed-2 0 1 :x 2 :y 3 :z 4)
					  (list 0 1 2 3 4))
				   (equal (key-fixed-4 0 1 2 3 :x 4 :y 5 :z 6)
					  (list 0 1 2 3 4 5 6))
				   (equal (key-fixed-6 0 1 2 3 4 5 :x 6 :y 7 :z 8)
					  (list 0 1 2 3 4 5 6 7 8)))))


(define-compiler-test "equality-1" t (lambda ()
				       (equal (list 1 2 3 4)
					      (list 1 2 3 4))))

(define-compiler-test "equality-2" nil (lambda ()
				       (equal (list 1 2 3 4)
					      (list 1 2 3))))

(define-compiler-test "equality-3" t (lambda ()
				       (equalp "bla" "BLA")))

(define-compiler-test "equality-4" t (lambda ()
				       (equalp (list "bla") (list "BLA"))))

(define-compiler-test "equality-4" nil (lambda ()
					 (equal (list "bla") (list "BLA"))))

(define-compiler-test "equality-5" t (lambda ()
				       (equalp #\a #\A)))


(define-compiler-test "equality-6" t (lambda ()
				       (let ((x 'foo)
					     (y 'foo))
					 (and (eq x y)
					      (eql x y)
					      (equal x y)
					      (equalp x y)
					      (equal (list x) (list y))
					      (equalp (list x x ) (list y y))))))

(define-compiler-test "math-1" t (lambda ()
				   (and (= 64 (ash 1 6))
					(= 1 (ash 64 -6)))))

(define-compiler-test "funcall-1" t (lambda ()
				     (equal (list 1 2 3)
					    (funcall (lambda (a b c) (list a b c))
						     1 2 3))))

(define-compiler-test "apply-1" t (lambda ()
				    (equal (list 1 2 3)
					   (apply (lambda (a b c) (list a b c))
						  1 (list 2 3)))))

(define-compiler-test "apply-stack-args" t (lambda ()
					     (equal (list 1 2 3 4 5 6 7)
						    (apply (lambda (a b c d e f g)
							     (list a b c d e f g))
							   1 (list 2 3 4 5 6 7)))))

(define-compiler-test "funcall-2" t (lambda ()
				      (let ((x (lambda () (lambda (a) (+ a a)))))
					(let ((y (funcall x)))
					  (= 20 (funcall y 10))))))

(define-compiler-test "funcall-3" t (lambda ()
				      (let ((a (lambda (x) (funcall x 1)))
					    (b (lambda () (lambda (x) (+ x x)))))
					(= 2 (funcall a (funcall b))))))

(define-compiler-test "mapcar-1" t (lambda ()
				     (equal
				      (mapcar (lambda (a b c)
						(+ a b c))
					      (list 1 2 3)
					      (list 10 20 30)
					      (list 100 200 300 400))
				      (list 111 222 333))))

(define-compiler-test "mapcar-2" t (lambda ()
				     (let ((x (lambda () 
						(lambda (x y)
						  (+ x y)))))
				       (equal '(11 22 33)
					      (mapcar (funcall x)
						      '(1 2 3)
						      '(10 20 30))))))

(define-compiler-test "mapcar-stack-args" t (lambda ()
				     (let ((x (lambda () 
						(lambda (x y)
						  (+ x y)))))
				       (equal '(11 22 33 44 55 66)
					      (mapcar (funcall x)
						      '(1 2 3 4 5 6)
						      '(10 20 30 40 50 60))))))


(defun generate-all-test-cores ()
  (let ((*debug* nil))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (funcall v))
	     *compiler-tests*)))
