(defun allocate-array (size tag)
  (declare (inline allocate-array))
  (allocate-array size tag))

(defun aref (array index)
  (declare (inline aref))
  (aref array index))

(defun setf-aref (array index value)
  (declare (inline setf-aref))
  (setf-aref array index value))

(defun make-array (size initial-content)
  (let ((array (allocate-array size (%compile-constant 1)))
	(index 0))
    (dolist (e initial-content)
      (setf-aref array index e)
      (setf index (+ 1 index)))
    array))

(defun array-total-size (array)
  (declare (inline array-total-size))
  (array-total-size array))

(defun make-string (size initial-content)
  (declare (notinline allocate-array setf-aref))
  (let ((array (allocate-array size (%compile-constant 2)))
	(index 0))
    (dolist (e initial-content)
      (setf-aref array index e)
      (setf index (+ 1 index)))
    array))
