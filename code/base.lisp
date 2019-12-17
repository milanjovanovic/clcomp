(defun eq (x y)
  (declare (inline eq))
  (eq x y))

(defun not (object)
  (if object
      nil
      t))

;;; FIXME
(defun eql (x y)
  (or (eq x y)))

(defun equal (x y)
  "FIXME")

(defun equalp (x y)
  "FIXME")
