(defun fixnump (x)
  (declare (inline fixnump))
  (fixnump x))

;;; for now we only have fixnums
(defun numberp (x)
  (declare (inline fixnump))
  (fixnump x))

(defun two-args-+ (a b)
  (declare (inline two-args-+))
  (two-args-+ a b))

(defun + (&rest rest)
  (declare (inline two-args-+))
  (let ((sum 0))
    (dolist (i rest)
      (setf sum (two-args-+ sum i)))
    sum))

(defun two-args-- (a b)
  (declare (inline two-args--))
  (two-args-- a b))

(defun %neg (a)
  (declare (inline %neg))
  (%neg a))

(defun - (num &rest rest)
  (declare (inline two-args--))
  (if rest
      (let ((result num))
	(dolist (i rest)
	  (setf result (two-args-- result i)))
	result)
      (%neg num)))

(defun two-args-= (a b)
  (declare (inline two-args-=))
  (two-args-= a b))

(defun = (num &rest rest)
  (declare (inline two-args-=))
  (dolist (i rest)
    (unless (two-args-= i num)
      (return-from = nil)))
  t)

(defun two-args-> (a b)
  (declare (inline two-args->))
  (two-args-> a b))

(defun > (num &rest rest)
  (dolist (i rest)
    (unless (> i num)
      (return-from > nil)))
  t)

(defun two-args-< (a b)
  (declare (inline two-args-<))
  (two-args-< a b))

(defun < (num &rest rest)
  (dolist (i rest)
    (unless (< i num)
      (return-from < nil)))
  t)

(defun two-args->= (a b)
  (declare (inline two-args->=))
  (two-args->= a b))

(defun >= (num &rest rest)
  (dolist (i rest)
    (unless (>= i num)
      (return-from >= nil)))
  t)

(defun two-args-<= (a b)
  (declare (inline two-args-<=))
  (two-args-<= a b))

(defun <= (num &rest rest)
  (dolist (i rest)
    (unless (<= i num)
      (return-from <= nil)))
  t)

(defun abs (x)
  (declare (inline abs))
  (abs x))

(defun two-args-logxor (x y)
  (declare (inline two-args-logxor))
  (two-args-logxor x y))

(defun two-args-logand (x y)
  (declare (inline two-args-logand))
  (two-args-logand x y))

(defun two-args-logior (x y)
  (declare (inline two-args-logior))
  (two-args-logior x y))

(defun logior (&rest rest)
  (declare (inline two-args-logior))
  (let ((sum 0))
    (dolist (i rest)
      (setf sum (two-args-logior sum i)))
    sum))

(defun logxor (&rest rest)
  (declare (inline two-args-logxor))
  (let ((sum 0))
    (dolist (i rest)
      (setf sum (two-args-logxor sum i)))
    sum))

(defun logand (&rest rest)
  (declare (inline two-args-logand))
  (let ((sum 0))
    (dolist (i rest)
      (setf sum (two-args-logand sum i)))
    sum))

(defun plusp (x)
  (declare (inline two-args->))
  (two-args-> x 0))

(defun minusp (x)
  (declare (inline two-args-<))
  (two-args-< x 0))

(defun zerop (z)
  (declare (inline two-args-=))
  (two-args-= z 0))

(defun ash (integer count)
  (declare (inline %fixnum->fixnum-shift-left %fixnum-shift-right))
  (if (= 0 count)
      integer
      (if (plusp count)
	  (%fixnum->fixnum-shift-left integer count)
	  (%fixnum-shift-right integer (abs count)))))


(defun byte (size position)
  (cons size position))

(defun ldb (bytespec integer)
  (let ((size (car bytespec))
	(start (cdr bytespec)))
    (logand (ash integer (- start))
	    (- (ash 1 size) 1))))

(defun evenp (number)
  (declare (inline %fixnum-evenp))
  (%fixnum-evenp number))

(defun oddp (number)
  (declare (inline %fixnum-evenp))
  (not (%fixnum-evenp number)))

(defun rem (n1 n2)
  (declare (inline %fixnum-fixnum-remainder))
  (%fixnum-fixnum-remainder n1 n2))

;; (defun dpb (newbyte bytespec integer)
;;   (logior (ash newbyte (cdr bytespec) integer)))

(defun %signed-ovflw-* (a b)
  (declare (inline %imul))
  (%imul a b))


;;;;;;;;;;;
(defun 1+ (x)
  (declare (inline two-args-+))
  (two-args-+ x 1))

(defun 1- (x)
  (declare (inline two-args--))
  (two-args-- x 1))

(defun max (number &rest rest)
  (declare (inline two-args->))
  (dolist (r rest)
    (when (two-args-> r number)
      (setf number r)))
  number)

(defun min (number &rest rest)
  (declare (inline two-args-<))
  (dolist (r rest)
    (when (two-args-< r number)
      (setf number r)))
  number)



