(defun %lisp-symbol-address (fun-name)
  (declare (inline %lisp-symbol-address))
  (%lisp-symbol-address fun-name))

(defun %c-call-zero-args  (c-fun)
  (declare (inline %c-call-zero-args))
  (%c-call-zero-args c-fun))

(defun %c-call-one-args  (c-fun arg1)
  (declare (inline %c-call-one-args))
  (%c-call-one-args c-fun arg1))

