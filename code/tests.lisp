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

(defstruct (baz (:include bar)) d)

(defstruct bla (a 1) (b 2))
(defstruct (cla (:include bla)) (c 10))

(defparameter *x* 100)

(defun baz (a b &optional c (d *x*))
  (list a b c d))

(defun mv-1 ()
  (values 1))

(defun mv-2 ()
  (values 1 2))

(defun mv-3 ()
  (values 1 2 3))

(defun mv-4 ()
  (values 1 2 3 4))

(defun mv-5 ()
  (values 1 2 3 4 5))

(defun mv-6 ()
  (values 1 2 3 4 5 6))

(defun mv-7 ()
  (values 1 2 3 4 5 6 7))

(defun mv-level-1-4 ()
  (mv-4))

(defun mv-level-2-4 ()
  (mv-level-1-4))

(defun mv-level-1-6 ()
  (mv-6))

(defun mv-level-2-6 ()
  (mv-level-1-6))

(defun mv-level-1-7 ()
  (mv-7))

(defun mv-level-2-7 ()
  (mv-level-1-7))
