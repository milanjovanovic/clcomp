(defun error (msg)
  (declare (inline %error))
  (%error msg))
