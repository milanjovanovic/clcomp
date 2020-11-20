(defun %raw (obj)
  (declare (inline %raw))
  (%raw obj))

(defun %apply (function args)
  (declare (inline %apply))
  (%apply function args))

(defun apply (function arg &rest arguments)
  (cond ((atom arguments)
	 (%apply function arg))
	((atom (cdr arguments))
	 (%apply function (cons arg (car arguments))))
	(t (do* ((a1 arguments a2)
		 (a2 (cdr arguments) (cdr a2)))
		((atom (cdr a2))
		 (rplacd a1 (car a2))
		 (%apply function (cons arg arguments)))))))

(defun funcall (fun &rest args)
  (apply fun args))

(defun type-of (x)
  (if (null x)
      'null
      (cond ((consp x) 'cons)
	    ((fixnump x) 'fixnum)
	    ((characterp x) 'character)
	    ((symbolp x) 'symbol)
	    ((stringp x) (list 'simple-array 'character (list (array-total-size x)))) ; FIXME, after fixing make-string/symbols
					; we can use %ARRAY-TYPE here
	    ((arrayp x) (%array-type x))
	    (t 'unknown))))

(defun typep (object type)
  "FIXME")

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
	((fixnump x) (abs x))
	((consp x) (%sxhash-cons x))
	((stringp x) (%sxhash-string x))
	((characterp x) (char-code x))
	((symbolp x) 100)
	((arrayp x) 200)
	(t (error "unknown type"))))

