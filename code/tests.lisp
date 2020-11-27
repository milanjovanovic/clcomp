(defun rest-fixed-0 (&rest x)
  (list x))

(defun rest-fixed-2 (a b &rest x)
  (list a b x))

(defun rest-fixed-3 (a b c &rest x)
  (list a b c x))

(defun rest-fixed-4 (a b c d &rest x)
  (list a b c d x))

(defun rest-fixed-5 (a b c d e &rest x)
  (list a b c d e x))

(defun rest-fixed-6 (a b c d e f &rest x)
  (list a b c d e f x))

(defun key-fixed-0 (&key x y)
  (list x y))

(defun key-fixed-1 (a &key x)
  (list a x))

(defun key-fixed-2 (a b &key x y z)
  (list a b x y z))

(defun key-fixed-4 (a b c d &key x y z)
  (list a b c d x y z))

(defun key-fixed-5 (a b c d e &key x y z)
  (list a b c d e x y z))

(defun key-fixed-6 (a b c d e f &key x y z)
  (list a b c d e f x y z))

(defun rest-fixed-7 (a b c d e f g &rest x)
  (list a b c d e f g x))

(defun rest-fixed-8 (a b c d e f g h &rest x)
  (list a b c d e f g h x))

(defun foo (&rest a &key b c)
  (list a b c))

(defun bar (a b &optional c (d 10))
  (list a b c d))

(defstruct foo a b)

(defstruct (bar (:include foo)) c)

(defparameter *x* 100)

(defun baz (a b &optional c (d *x*))
  (list a b c d))

