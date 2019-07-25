(defun make-array (size initial-content)
  (let ((array (a-allocate-array size))
	(index 0))
    (dolist (e initial-content)
      (a-setf-aref array index e)
      (setf index (+ 1 index)))))

(defun a-allocate-array (size)
  (allocate-array size))

(defun a-aref (array index)
  (aref array index))

(defun a-setf-aref (array index value)
  (setf-aref array index value))
