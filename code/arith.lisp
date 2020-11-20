(defun fixnump (x)
  (declare (inline fixnump))
  (fixnump x))

(defun two-args-+ (a b)
  (declare (inline two-args-+))
  (two-args-+ a b))

(defun + (&rest rest)
  (let ((sum 0))
    (dolist (i rest)
      (setf sum (+ sum i)))
    sum))

(defun two-args-- (a b)
  (declare (inline two-args--))
  (two-args-- a b))

(defun %neg (a)
  (declare (inline %neg))
  (%neg a))

(defun - (num &rest rest)
  (if rest
      (let ((result num))
	(dolist (i rest)
	  (setf result (- result i)))
	result)
      (%neg num)))

(defun two-args-= (a b)
  (declare (inline two-args-=))
  (two-args-= a b))

(defun = (num &rest rest)
  (dolist (i rest)
    (unless (= i num)
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

;; FIXME, use vop
(defun two-args->= (a b)
  (declare (inline two-args-= two-args->))
  (or (two-args-= a b) (two-args-> a b)))

(defun >= (num &rest rest)
  (dolist (i rest)
    (unless (>= i num)
      (return-from >= nil)))
  t)

;; FIXME, use VOP
(defun two-args-<= (a b)
  (declare (inline two-args-= two-args-<))
  (or (two-args-= a b) (two-args-< a b)))

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

(defun logxor (x y)
  (logxor x y))

(defun logand (x y)
  (logand x y))

(defun plusp (x)
  (declare (inline %fixnum-larger-than-zero))
  (%fixnum-larger-than-zero x))

(defun minusp (x)
  (declare (inline %fixnum-less-than-zero))
  (%fixnum-less-than-zero x))

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

;; (defun dpb (newbyte bytespec integer)
;;   (logior (ash newbyte (cdr bytespec) integer)))

(defun %signed-ovflw-* (a b)
  (declare (inline %imul))
  (%imul a b))
