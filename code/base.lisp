(defun funcall (fun &rest args)
  (apply fun args))

(defun apply (fun arg &rest args)
  "FIXME")

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

(defun not (object)
  (if object
      nil
      t))

;;; FIXME
;; (defun eql (x y)
;;   (let ((xt (type-of x))
;; 	(xy (type-of y)))
;;     (if (eq xt xy)
;; 	(case xt
;; 	  ((fixnum character symbol)))
;; 	nil)))

(defun equal (x y)
  "FIXME")

(defun equalp (x y)
  "FIXME")

(defun sxhash (x)
  "FIXME")














