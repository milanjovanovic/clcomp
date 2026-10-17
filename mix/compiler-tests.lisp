(in-package :clcomp)

(defun dump-core (file form)
  (let ((*debug* nil)
	(*load-time-fixups* nil))
    (setf *structs* nil)
    (setf *defsetfs* nil)
    (setf *dynamic-variables* nil)
    (rt-reset)
    (process-bootstrap-data)
    (process-code-stubs)
    (clcomp-compile-file (format nil "~a/code/global.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/base.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/cons.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/array.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/seq.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/arith.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/ffi.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/symbol.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/char.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/runtime/runtime-support.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/struct.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/io.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/tests.lisp" *clcomp-home*))
    (set-start-address (clcomp-compile nil form))
    (let* ((bootstrap-data-list (make-bootstrap-data))
	   (code-buffers-list (make-compilation-binary-data))
	   (fixups-buffers-list (make-fixups-code-buffers-list))
	   (fixups-size (* *word-size* (length fixups-buffers-list))))
      (with-open-file (f file :direction :output
			      :if-exists :supersede
			      :element-type '(unsigned-byte 8))

	(dolist (c (immediate-as-byte-list fixups-size :imm64))
	  (write-byte c f))
	(dolist (fixup-data-buffer fixups-buffers-list)
	  (write-sequence fixup-data-buffer f))
	(write-start-address f)
	(dolist (bootstrap-data bootstrap-data-list)
	  (write-sequence bootstrap-data f))
	(dolist (stub *code-stubs-data*)
	  (write-sequence (second stub) f))
	(dolist (code-buffer code-buffers-list)
	  (write-sequence  code-buffer f))))))

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


(define-compiler-test "fixnum-test-1" t (lambda ()
					  (and
					   (= 1007616 (ash 123 13))
					   (= -819200 (ash -100 13))
					   (> 1 0)
					   (< -5 -3)
					   (< -5 5)
					   (= 11 (+ -10 21)))))

(define-compiler-test "fixnum-test-1" t (lambda ()
					  (and
					   (= -1 (- 1))
					   (= 0 (- 0))
					   (= -12 (- 2 14))
					   (= 10 (abs -10) (abs 10))
					   (= -1192 (logxor 123 -1245) )
					   (plusp 1)
					   (not (plusp 0))
					   (not (plusp -1))
					   (minusp -1)
					   (not (minusp 0)))))

(define-compiler-test "arith-fixnump" t (lambda ()
                                          (and (fixnump 0)
                                               (fixnump -1)
                                               (fixnump 4611686018427387900)
                                               (fixnump -4611686018427387900)
                                               (fixnump (char-code #\a))
                                               (not (fixnump #\a))
                                               (not (fixnump nil))
                                               (not (fixnump t))
                                               (not (fixnump "foo"))
                                               (not (fixnump (cons 1 2)))
                                               (fixnump (car (cons 1 "foo"))))))

(define-compiler-test "arith-evenp-oddp" t (lambda ()
                                             (and (evenp 0)
                                                  (evenp 2)
                                                  (evenp 4)
                                                  (evenp 6)
                                                  (evenp -4)
                                                  (evenp 4611686018427387900)
                                                  (not (evenp 3))
                                                  (oddp 3)
                                                  (oddp 5)
                                                  (oddp -3)
                                                  (oddp 4611686018427387899)
                                                  (not (oddp 8)))))

(define-compiler-test "arith-logior" t (lambda ()
                                         (and (= 3 (logior 1 2))
                                              (= 12 (logior 8 4))
                                              (= 7 (logior 5 2))
                                              (= -1 (logior -1 0))
                                              (= -5 (logior -5 0)))))



(define-compiler-test "arith-compare-2args" t (lambda ()
                                                (and (> 4611686018427387900 -4611686018427387900)
                                                     (< -4611686018427387900 4611686018427387900)
                                                     (= 4611686018427387900 4611686018427387900)
                                                     (not (= 4611686018427387900 4611686018427387899))
                                                     (> 0 -1)
                                                     (< -1 0)
                                                     (not (> -1 0))
                                                     (>= 5 5)
                                                     (<= 5 5)
                                                     (not (>= 5 6))
                                                     (not (<= 6 5)))))

(define-compiler-test "arith-abs" t (lambda ()
                                      (and (= 0 (abs 0))
                                           (= 10 (abs -10) (abs 10))
                                           (= 4611686018427387900 (abs -4611686018427387900))
                                           (= 4611686018427387900 (abs 4611686018427387900))
                                           (= 5 (abs (%neg 5))))))


(define-compiler-test "arith-neg" t (lambda ()
                                      (and (= -1 (- 1))
                                           (= 0 (- 0))
                                           (= 5 (- -5))
                                           (= -5 (- 5))
                                           (= -4611686018427387900 (%neg 4611686018427387900)))))

(define-compiler-test "arith-plusp-minusp" t (lambda ()
                                               (and (plusp 1)
                                                    (plusp 4611686018427387900)
                                                    (not (plusp 0))
                                                    (not (plusp -1))
                                                    (minusp -1)
                                                    (minusp -4611686018427387900)
                                                    (not (minusp 0))
                                                    (not (minusp 1)))))

(define-compiler-test "arith-logand-logxor" t (lambda ()
                                                (and (= 6 (logxor 5 3))
                                                     (= 0 (logxor 7 7))
                                                     (= -1192 (logxor 123 -1245))
                                                     (= -8 (logxor -5 3))
                                                     (= -1 (logxor -1 0))
                                                     (= 7 (logand -1 7))
                                                     (= 6 (logand -2 7))
                                                     (= 0 (logand 12 2)))))

(define-compiler-test "arith-rest-args" t (lambda ()
                                            (and (= 21 (+ 1 2 3 4 5 6))
                                                 (= 85 (- 100 1 2 3 4 5))
                                                 (= 0 (+))
                                                 (= -7 (- 7))
                                                 (= 9 9 9 9 9 9 9))))



;; FIXME, this doesn't work
#+nil
(define-compiler-test "arith-rem" t (lambda ()
                                      (and (= 1 (rem 7 3))
                                           (= -1 (rem -7 3))
                                           (= 1 (rem 7 -3))
                                           (= -1 (rem -7 -3))
                                           (= 0 (rem 0 5))
                                           (= 5 (rem 5 7))
                                           (= 0 (rem 4611686018427387900 7))
                                           (= -6 (rem -4611686018427387899 7))
                                           (minusp (rem -7 3)))))

(define-compiler-test "arith-ash-right" t (lambda ()
                                            (and (= 1 (ash 64 -6))
                                                 (= 4 (ash 64 -4))
                                                 (= 3 (ash 7 -1))
                                                 (= -4 (ash -7 -1))
                                                 (= -1 (ash -64 -6))
                                                 (= -16 (ash -256 -4))
                                                 (= 128 (ash (ash 1 10) -3)))))


;;; cons/list
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

;;; CHAR
(define-compiler-test "char-test-1" t (lambda ()
					(let* ((c1 #\a)
					       (c2 (char-upcase c1)))
					  (and (= (char-code c1) 97)
					       (= (char-code c2) 65)))))

(define-compiler-test "char-test-1" t (lambda ()
					(let* ((c1 #\a)
					       (c2 (char-upcase c1)))
					  (and (not (equal c1 c2))
					       (equalp c1 c2)))))

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


(define-compiler-test "struct-test-1" t (lambda ()
					  (let ((foo (make-foo :a 1 :b 2))
						(bar (make-bar :a 10 :b 20 :c 30)))
					    (and
					     (= (foo-a bar) 10)
					     (= (foo-b bar) 20)
					     (= (bar-c bar) 30)
					     (= (foo-a foo) 1)
					     (= (foo-b foo) 2)))))

(define-compiler-test "struct-test-2" t (lambda ()
					  (let ((foo (make-foo :a 1 :b 2))
						(bar (make-bar :a 10 :b 20 :c 30))
						(baz (make-baz :a 100 :b 200 :c 300)))
					    (and
					     (foo-p foo)
					     (foo-p bar)
					     (foo-p baz)
					     (bar-p bar)
					     (bar-p baz)
					     (baz-p baz)
					     (null (baz-d baz))
					     (= 300 (bar-c baz))
					     (= 100 (foo-a baz))))))


(define-compiler-test "dynamic-var-1" t (lambda ()
					  (let ((x (baz 1 2 3)))
					    (equal x (list 1 2 3 100)))))

(define-compiler-test "optional-1" t (lambda ()
				       (and
					(equal (bar 1 2 3) (list 1 2 3 10))
					(equal (bar 1 2 3 4) (list 1 2 3 4)))))

(define-compiler-test "bootstrap-symbols-1" t (lambda ()
						(and
						 (eq 'character 'character)
						 (eq 'simple-array 'simple-array)
						 (equal "CL" (symbol-package 'character))
						 (equal "CL" (symbol-package 'simple-array)))))


(define-compiler-test "quoted-objects-1" t (lambda ()
					     (and
					      (eq ':foo :foo)
					      (equal '"bla" "bla")
					      (eq nil 'nil)
					      (eq t 't))))


(define-compiler-test "bootstrap-symbols-2" t (lambda ()
						(and
						 (not (eq :foo 'foo))
						 (not (eq 'character :character))
						 (not (eq :element-type 'element-type))
						 (eq 'element-type 'element-type)
						 (eq :element-type :element-type)
						 (equal "KEYWORD" (symbol-package :element-type))
						 (equal "CL" (symbol-package 'element-type)))))

(define-compiler-test "closures-1" t (lambda ()
				       (let* ((a 1)
					      (f (lambda (x) x)))
					 (= 1 (funcall f 1)))))

(define-compiler-test "closures-2" t (lambda ()
				       (let* ((a 1)
					      (f (lambda (x) (+ a x))))
					 (= 2 (funcall f 1)))))

(define-compiler-test "closures-3" nil (lambda ()
					 (let* ((a 1)
						(f (lambda (x) (+ a x))))
					   (= 3 (funcall f 1)))))

(define-compiler-test "closures-4" t (lambda ()
				       (let* ((a 1)
					      (f (lambda ()
						   (lambda () (+ 1 a))))
					      (f2 (funcall f)))
					 (= (funcall f2) 2))))

(define-compiler-test "closures-5" t (lambda ()
				       (let* ((x (list 1 2))
					      (f (lambda () (setf (car x) 10))))
					 (funcall f)
					 (equal (list 10 2)
						x))))

(define-compiler-test "closures-6" t (lambda ()
				       (let* ((x (list 1 2))
					      (f (lambda () (setf (car x) 10)))
					      (f2 (lambda () (setf (car x) 100))))
					 (funcall f)
					 (funcall f2)
					 (equal (list 100 2)
						x))))

;;; MV
(define-compiler-test "mv-1" t (lambda ()
                                 (multiple-value-bind (a b)  (values 1 2)
                                   (and (= a 1)
                                        (= b 2)))))

(define-compiler-test "mv-2" t (lambda ()
                                 (multiple-value-bind (a b c d)  (values 1 2)
                                   (and (= a 1)
                                        (= b 2)
                                        (null c)
                                        (null d)))))

(define-compiler-test "mv-3" t (lambda ()
                                 (multiple-value-bind (a b c d)  (mv-level-2-4)
                                   (and (= a 1)
                                        (= b 2)
                                        (= c 3)
                                        (= d 4)))))

(define-compiler-test "mv-4" t (lambda ()
                                 (multiple-value-bind (a b c d e f)  (mv-level-2-4)
                                   (and (= a 1)
                                        (= b 2)
                                        (= c 3)
                                        (= d 4)
                                        (null e)
                                        (null f)))))

(define-compiler-test "mv-5" t (lambda ()
                                 (multiple-value-bind (a b c d e f g)  (mv-level-2-6)
                                   (and (= a 1)
                                        (= b 2)
                                        (= c 3)
                                        (= d 4)
                                        (= e 5)
                                        (= f 6)
                                        (null g)))))

(define-compiler-test "mv-6" t (lambda ()
                                 (multiple-value-bind (a b c d e f g h)  (mv-level-2-7)
                                   (and (= a 1)
                                        (= b 2)
                                        (= c 3)
                                        (= d 4)
                                        (= e 5)
                                        (= f 6)
                                        (= g 7)
                                        (null h)))))

(defun generate-all-test-cores ()
  (let ((*debug* nil))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (funcall v))
	     *compiler-tests*)))
