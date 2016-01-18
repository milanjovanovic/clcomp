(in-package #:clcomp)

(defun allocate-heap (size)
  (sb-alien:alien-funcall (sb-alien:extern-alien "allocate_heap" (sb-alien:function sb-alien:long sb-alien:int))
			  size))

(defun load-code (code)
  (let* ((code-buffer (sb-alien:make-alien (sb-alien:unsigned 8) (length code)))
	 (code-buffer-sap (sb-alien:alien-sap code-buffer)))
    (loop for b across code
	  for index from 0
	  do
	     (setf (sb-sys:sap-ref-32 code-buffer-sap index) (aref code index)))
    (sb-alien:alien-funcall (sb-alien:extern-alien "load_code" (sb-alien:function sb-alien:int sb-alien:int (* sb-alien:char)))
			    (length code)
			    code-buffer-sap)
    (sb-alien:free-alien code-buffer)))
