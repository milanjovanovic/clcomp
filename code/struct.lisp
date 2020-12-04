(defun %allocate-struct (slot-count type layout)
  (declare (inline %allocate-struct))
  (%allocate-struct slot-count type layout))

(defun %struct-type (struct)
  (declare (inline %struct-type))
  (%struct-type struct))

(defun %struct-layout (struct)
  (declare (inline %struct-layout))
  (%struct-layout struct))

(defun %structp (obj)
  (declare (inline %structp))
  (%structp obj))

(defun %get-struct-slot (struct slot-index)
  (declare (inline %get-struct-slot))
  (%get-struct-slot struct slot-index))

(defun %set-struct-slot (struct slot-index value)
  (declare (inline %set-struct-slot))
  (%set-struct-slot struct slot-index value))

(defun %check-struct-type (instance type)
  (unless (or (eq type (%struct-type instance))
	      (%%struct-layout-has-type type (%struct-layout instance)))
    (error (concatenate 'string "Argument is not a " (symbol-name type)))))
