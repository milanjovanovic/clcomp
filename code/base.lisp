(defun %raw (obj)
  (declare (inline %raw))
  (%raw obj))

(defun %apply (function args)
  (declare (inline %apply))
  (%apply function args))

(defun apply (function arg &rest arguments)
  (let ((function (if (symbolp function)
		      (symbol-function function)
		      function)))
    (cond ((atom arguments)
	   (%apply function arg))
	  ((atom (cdr arguments))
	   (%apply function (cons arg (car arguments))))
	  (t (do* ((a1 arguments a2)
		   (a2 (cdr arguments) (cdr a2)))
		  ((atom (cdr a2))
		   (rplacd a1 (car a2))
		   (%apply function (cons arg arguments))))))))

(defun funcall (fun &rest args)
  (if (symbolp fun)
      (apply (symbol-function fun) args)
      (apply fun args)))

(defun type-of (x)
  (if (null x)
      'null
      (cond ((eq x t) t)
	    ((consp x) 'cons)
	    ((fixnump x) 'fixnum)
	    ((characterp x) 'character)
	    ((keywordp x) 'keyword)
	    ((symbolp x) 'symbol)
	    ((stringp x) (list 'simple-array 'character (list (array-total-size x)))) ; FIXME, after fixing make-string/symbols
					; we can use %ARRAY-TYPE here
	    ((arrayp x) (%array-type x))
	    (t 'unknown))))

;;;FIXME, just simple implementation for now
(defun typep (object type)
  (if (consp type)
      (error "Not implemented yet")
      (if (eq type t)
	  t
	  (cond 
	    ((or (eq type 'fixnum)
		 (eq type 'number)
		 (eq type 'integer)) (fixnump object)) ;; FIXME, for now we only have fuxnum
	    ((eq type 'cons) (consp object))
	    ((eq type 'list) (listp object))
	    ((eq type 'keyword) (keywordp object))
	    ((eq type 'symbol) (symbolp object))
	    ((eq type 'array) (arrayp object))
	    ;; FIXME, what about STRUCTURES here ???
	    ((%structp object) "FIXME")
	    (t (eq type (type-of object)))))))

;;; FIXME, should be a macro
(defun check-type (object type)
  (error "Not implemented yet"))

(defun eq (x y)
  (declare (inline eq))
  (eq x y))

;; FIXME, after bignum/floats implementation this need to be changed
(defun eql (x y)
  (declare (inline eq))
  (eq x y))

(defun not (object)
  (if object
      nil
      t))

;;; FIXME, ARRAY
(defun equal (x y)
  (or (eq x y)
      (cond ((and (consp x) (consp y))
	     (and (equal (car x) (car y))
		  (equal (cdr x) (cdr y))))
	    ((and (stringp x) (stringp y))
	     (string= x y)))))

;;; FIXME, ARRAY
(defun equalp (x y)
  (or (eq x y)
      (cond ((and (consp x) (consp y))
	     (and (equalp (car x) (car y))
		  (equalp (cdr x) (cdr y))))
	    ((and (stringp x) (stringp y))
	     (string-equal x y))
	    ((and (characterp x) (characterp y))
	     (char-equal x y)))))

(defun sxhash (x)
  (cond ((null x) 0)
	((eq x t) 1)
	((fixnump x) (abs x))
	((consp x) (%sxhash-cons x))
	((stringp x) (%sxhash-string x))
	((characterp x) (char-code x))
	((symbolp x) 100)
	((arrayp x) 200)
	(t (error "Unknown type"))))


(defun in-package (package)
  package)
