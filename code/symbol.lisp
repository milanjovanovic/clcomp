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

(defun %defparameter (symbol value)
  (declare (inline set-symbol-value))
  (set-symbol-value symbol value))

(defun %make-and-intern-symbol (name)
  (let ((symbol (make-symbol name)))
    (%add-to-interned-symbols symbol)
    symbol))

(defun intern (name)
  (let ((symbol (%get-global-interned-symbol name)))
    (or symbol
	(%make-and-intern-symbol name))))
