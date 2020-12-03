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

(defun symbol-package (symbol)
  (declare (inline symbol-package))
  (symbol-package symbol))

(defun symbolp (symbol)
  (declare (inline symbolp))
  (or (eq t symbol)
      (eq nil symbol)
      (symbolp symbol)))

(defun keywordp (symbol)
  (and (symbolp symbol)
       (equal (symbol-package symbol) "KEYWORD")))

(defun %set-symbol-value (symbol value)
  (declare (inline %set-symbol-value))
  (%set-symbol-value symbol value))

(defun %set-symbol-function (symbol value)
  (declare (inline %set-symbol-function))
  (%set-symbol-function symbol value))

(defun %defparameter (symbol value)
  (declare (inline %set-symbol-value))
  (%set-symbol-value symbol value))

(defun %set-symbol-package (symbol package)
  (declare (inline %set-symbol-package))
  (%set-symbol-package symbol package))

(defun %make-and-intern-symbol (name package)
  (let ((symbol (make-symbol name)))
    (%add-to-interned-symbols symbol package)
    symbol))

;;; FIXME, package is current a string
(defun intern (name &optional (package *package*))
  (let ((symbol (%get-global-interned-symbol name package)))
    (or symbol
	(let ((sym (%make-and-intern-symbol name package)))
	  (%set-symbol-package sym package)
	  sym))))
