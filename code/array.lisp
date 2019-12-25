(defun arrayp (o)
  (declare (inline arrayp))
  (arrayp o))

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

(defun %copy-array (array)
  (declare (inline %copy-array))
  (%copy-array array))


;;; STRING
;;; FIXME, doesn't work when initial-content is list of NIL's
;;; Maybe DOLIST expansion is wrong
(defun make-string (size initial-content)
  (declare (notinline allocate-array setf-aref))
  (let ((array (allocate-array size (%compile-constant 2)))
	(index 0))
    (dolist (e initial-content)
      (setf-aref array index e)
      (setf index (+ 1 index)))
    array))

(defun char (string index)
  (declare (inline aref))
  (aref string index))

(defun setf-char (string index value)
  (setf (aref string index) value))

(defun nstring-downcase (string)
  (dotimes (i (array-total-size string))
    (setf (char string i)
	  (char-downcase (char string i))))
  string)

(defun nstring-upcase (string)
  (dotimes (i (array-total-size string))
    (setf (char string i)
	  (char-upcase (char string i))))
  string)

(defun string-downcase (string)
  (nstring-downcase (%copy-array string)))

(defun string-upcase (string)
  (nstring-upcase (%copy-array string)))

(defun string= (s1 s2)
  (and (= (length s1)
	  (length s2))
       (progn
	 (dotimes (index (length s1))
	   (unless (char= (char s1 index)
			  (char s2 index))
	     (return-from string= nil)))
	 t)))

(defun string-equal (s1 s2)
  (and (= (length s1)
	  (length s2))
       (progn
	(dotimes (index (length s1))
	  (unless (char-equal (char s1 index)
			      (char s2 index))
	    (return-from string-equal nil)))
	t)))

;;; FIXME, implement this with REPLACE
(defun concatenate (type &rest strings)
  (let ((size 0)
	(current-index 0))
    (dolist (s strings)
      (setf size (+ size (length s))))
    (let ((rstring (make-string size nil)))
      (dolist (s strings)
	(dotimes (i (length s))
	  (setf (char rstring current-index)
		(char s i))
	  (setf current-index (+ current-index 1))))
      rstring)))
