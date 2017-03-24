(in-package #:clcomp)

(defun load-runtime ()
  (sb-alien:load-shared-object (concatenate 'string
					    (directory-namestring (asdf:component-pathname (asdf:find-system "clcomp")))
					    "c/runtime.so")))

(defun allocate-heap (size)
  (sb-alien:alien-funcall (sb-alien:extern-alien "allocate_heap" (sb-alien:function sb-alien:long sb-alien:int))
			  size))


(defun load-code (code)
  (let* (( SB-ALIEN-INTERNALS:*VALUES-TYPE-OKAY* t)
	 (code-buffer (sb-alien:make-alien (sb-alien:unsigned 8) (length code)))
	 (code-buffer-sap (sb-alien:alien-sap code-buffer)))
    (loop for b across code
	  for index from 0
	  do
	     (setf (sb-sys:sap-ref-32 code-buffer-sap index) (aref code index)))
    (sb-alien:alien-funcall (sb-alien:extern-alien "load_code" (sb-alien:function sb-alien:void (* sb-alien:char) sb-alien:int))
			    code-buffer-sap
			    (length code))
    (sb-alien:free-alien code-buffer)))
