(defun copy-seq (what)
  "FIXME")

(defun length (seq)
  (if (listp seq)
      (list-length seq)
      (array-total-size seq)))
