(defun allocate-array (size)
  (allocate-array size))

(defun aref (array index)
  (aref array index))

(defun setf-aref (array index value)
  (setf-aref array index value))

(defun make-array (size initial-content)
  (declare (notinline allocate-array setf-aref))
  (let ((array (allocate-array size))
	(index 0))
    (dolist (e initial-content)
      (setf-aref array index e)
      (setf index (+ 1 index)))
    array))
