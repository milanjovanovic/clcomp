(in-package :clcomp)

(defparameter *structs* nil)

(defparameter *defsetfs* nil)

(defparameter *macros* (make-hash-table))

(defun %%get-struct-info (struct)
  (assoc struct *structs* ))

(defun %%define-struct (name parent slots)
  (let ((struct-info (%%get-struct-info name)))
    (if struct-info
	(rplacd struct-info (list parent slots))
	(push (list name parent slots) *structs*))))

(defun %%get-struct-slots (struct)
  (let ((struct-data (assoc struct *structs*)))
    (when struct-data
      (nth 2 struct-data))))

(defun %%compiler-defun (fun-name)
  (declare (ignore fun-name)))

(defun %%defsetf (getter setter)
  (let ((current (assoc getter *defsetfs*)))
    (if current
	(setf (cdr current) setter)
	(push (cons getter setter) *defsetfs*))))
