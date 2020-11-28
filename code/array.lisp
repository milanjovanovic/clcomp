(defun arrayp (o)
  (declare (inline %pointerp arrayp))
  (and (%pointerp o)
       (arrayp o)))

(defun allocate-array (size)
  (declare (inline allocate-array))
  (allocate-array size))

(defun aref (array index)
  (declare (inline aref))
  (aref array index))

(defun setf-aref (array index value)
  (declare (inline setf-aref))
  (setf-aref array index value))

(defun %array-type (array)
  (declare (inline %array-type))
  (%array-type array))

;;; FIXME, multi-dimensional arrays
(defun make-array-new (dimensions &key (element-type t) (initial-element 0) initial-contents adjustable)
  (declare (inline %set-array-type %set-array-element-type %set-array-tag debug
		   %set-simple-array-tag))
  (let ((array (allocate-array dimensions)))
    (%set-array-type array (list 'simple-array element-type (list dimension)))
    (%set-array-element-type array element-type)
    (%set-simple-array-tag array)	; FIXME, no need for VOP here
    (if initial-contents
	(let ((index 0))
	  (dolist (e initial-contents)
	    (setf-aref array index e)
	    (setf index (+ 1 index))))
	(dotimes (i dimensions)
	  (setf-aref array i initial-element)))
    array))

;;; FIXME, &KEY arguments
(defun make-array (dimension element-type initial-element initial-contents)
  (declare (inline %set-array-type %set-array-element-type %set-array-tag debug
		   %set-simple-array-tag))
  (let ((array (allocate-array dimension)))
    (%set-array-type array (list 'simple-array element-type (list dimension)))
    (%set-array-element-type array element-type)
    (%set-simple-array-tag array) ; FIXME, no need for VOP here
    (if initial-contents
	(let ((index 0))
	  (dolist (e initial-contents)
	    (setf-aref array index e)
	    (setf index (+ 1 index))))
	(dotimes (i dimension)
	  (setf-aref array i initial-element)))
    array))

(defun array-total-size (array)
  (declare (inline array-total-size))
  (array-total-size array))

(defun %copy-array (array)
  (declare (inline %copy-array))
  (%copy-array array))

;;; STRING

(defun stringp (o)
  (declare (inline stringp))
  (stringp o))

(defun make-string (size &key initial-element element-type)
  (declare (inline %set-array-type %set-array-element-type %set-array-tag debug %set-string-tag))
  (let ((array (allocate-array size)))
    (%set-array-type array (list 'simple-array 'character (list size)))
    (%set-array-element-type array 'character)
    (%set-string-tag array)		; FIXME, no need for VOP here
    (dotimes (index size)
      (setf-aref array index initial-element)) 
    array))

(defun %char-list-to-string (size initial-content)
  (let ((string (make-string size))
	(index 0))
    (dolist (e initial-content)
      (setf-aref string index e)
      (setf index (+ index 1)))
    string))

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

(defun %sxhash-string (string)
  (declare (inline abs))
  (let ((hash 0)
	(string-size (array-total-size string)))
    (dotimes (i string-size)
      (setf hash (+ (%signed-ovflw-* 31 hash) (char-code (char string i)))))
    (abs hash)))
