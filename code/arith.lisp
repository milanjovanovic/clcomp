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

(defun - (num &rest rest)
  (if rest
      (let ((result num))
	(dolist (i rest)
	  (setf result (- result i)))
	result)))

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
