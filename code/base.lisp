(defun %raw (obj)
  (declare (inline %raw))
  (%raw obj))

(defun apply (fun arg &rest args)
  (declare (inline %apply))
  (cond ((null args)
	 (%apply fun arg))
	(t
	 (%apply fun (cons arg args)))))

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

;; (defun sxhash (x)
;;   (cond ((fixnum x) x)
;; 	((stringp x) "")
;; 	((characterp x) "")
;; 	((symbolp x) symbol)))

