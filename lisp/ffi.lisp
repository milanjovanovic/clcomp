(defun allocate-heap (size)
  (sb-alien:alien-funcall (sb-alien:extern-alien "allocate_heap" (function long int))
			  size))

(defun load-code (code)
  (let* ((code-buffer (sb-alien:make-alien (unsigned 8) (length code)))
	 (code-buffer-sap (sb-alien:alien-sap code-buffer)))
    (loop for b across code
	  for index from 0
	  do
	     (setf (sb-sys:sap-ref-32 code-buffer-sap index) (aref code index)))
    (sb-alien:alien-funcall (sb-alien:extern-alien "load_code" (function int int (* char)))
			    (length code)
			    code-buffer-sap)
    (sb-alien:free-alien code-buffer)))
