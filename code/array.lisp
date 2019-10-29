(defun allocate-array (size tag)
  (allocate-array size tag))

(defun aref (array index)
  (aref array index))

(defun setf-aref (array index value)
  (setf-aref array index value))

(defun make-array (size initial-content)
  (declare (notinline allocate-array setf-aref))
  (let ((array (allocate-array size (%compile-constant 1)))
	(index 0))
    (dolist (e initial-content)
      (setf-aref array index e)
      (setf index (+ 1 index)))
    array))

(defun make-string (size initial-content)
  (declare (notinline allocate-array setf-aref))
  (let ((array (allocate-array size (%compile-constant 2)))
	(index 0))
    (dolist (e initial-content)
      (setf-aref array index e)
      (setf index (+ 1 index)))
    array))


