(defun make-symbol (name)
  (declare (inline make-symbol))
  (make-symbol name))

(defun symbol-name (symbol)
  (declare (inline symbol-name))
  (symbol-name symbol))

(defun symbol-value (symbol)
  (declare (inline symbol-value))
  (symbol-value symbol))

(defun symbol-function (symbol)
  (declare (inline symbol-function))
  (symbol-function symbol))

(defun symbol-plist (symbol)
  (declare (inline symbol-plist))
  (symbol-plist symbol))

(defun symbolp (symbol)
  (declare (inline symbolp))
  (symbolp symbol))

;;; FIXME, implement interned symbols storageintern
(defun %intern-symbol (name symbol)
  ;; FIXME
  symbol
  )

(defun %make-and-intern-symbol (name)
  (let ((symbol (make-symbol name)))
    (%intern-symbol name symbol)))

(defun %get-interned-symbol (name)
  ;; FIXME
  (make-symbol name))

(defun intern (name)
  (let ((symbol (%get-interned-symbol name)))
    (or symbol
	(%make-and-intern-symbol name))))
