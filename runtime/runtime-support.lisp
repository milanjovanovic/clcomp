(in-package :clcomp)

(defparameter *structs* nil)

(defparameter *defsetfs* nil)

(defparameter *dynamic-variables* nil)

(defun %%get-struct-info (struct)
  (assoc struct *structs* ))

(defun %%make-all-parents-list (struct)
  (let* ((struct-info (%%get-struct-info struct))
	 (parents (second struct-info)))
    (if parents
	(cons struct parents)
	(list struct))))

(defun %%define-struct (name parent slots)
  (let ((struct-info (%%get-struct-info name))
	(all-parents (when parent
		       (%%make-all-parents-list parent))))
    (if struct-info
	(rplacd struct-info (list (list all-parents slots)))
	(setf *structs* (cons (list name all-parents slots) *structs*)))))

(defun %%get-struct-parents (struct)
  (let ((struct-data (assoc struct *structs*)))
    (when struct-data
      (nth 1 struct-data))))

(defun %%compiler-defun (fun-name)
  (declare (ignore fun-name)))

(defun %%defsetf (getter setter)
  (let ((current (assoc getter *defsetfs*)))
    (if current
	(setf (cdr current) setter)
	(setf *defsetfs* (cons (cons getter setter) *defsetfs*)))))

(defun %%compiler-defparameter (s)
  (unless (find s *dynamic-variables*)
    (setf *dynamic-variables*
	  (cons s *dynamic-variables*))))

;; FIXME, use MEMBER
(defun %%struct-layout-has-type (type layout)
  (find type layout))
