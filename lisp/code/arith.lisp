(defun = (a b)
  (= a b))

(defun > (a b)
  (> a b))

(defun < (a b)
  (< a b))

(defun >= (a b)
  (or (= a b) (> a b)))

(defun <= (a b)
  (or (= a b) (< a b)))
