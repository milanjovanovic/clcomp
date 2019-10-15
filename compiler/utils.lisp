(in-package #:clcomp)

(defun print-bits (n size)
  (format t "~B" (ldb (byte size 0) n)))


(defun byte-hex (a)
  (format nil "~2,'0x" a))

(defun print-byte (n)
  (format t "#b~8,'0b" n)
  (format t "~%#x~2,'0x" n)
  (format t "~%~D" n))

(defparameter *byte-min* (- (expt 2 7)))
(defparameter *byte-max* (- (expt 2 7) 1))
(defparameter *word-min* (- (expt 2 15)))
(defparameter *word-max* (- (expt 2 15) 1))
(defparameter *dword-min* (- (expt 2 31)))
(defparameter *dword-max* (- (expt 2 31) 1))
(defparameter *qword-min* (- (expt 2 63)))
(defparameter *qword-max* (- (expt 2 63) 1))

(defun byte-as-byte-list (byte)
  (list byte))

(defun dword-as-byte-list (dword)
  (let ((res nil))
    (dotimes (i 4)
      (push (ldb (byte 8 (* i 8)) dword) res))
    res))

(defun qword-as-byte-list (qword)
  (let ((res nil))
    (dotimes (i 8)
      (push (ldb (byte 8 (* i 8)) qword) res))
    res))

(defun immediate-as-byte-list (immediate template)
  (let ((bits (immediate-bits template))
	(signed-immediate (make-signed-immediate immediate template)))
    (let ((res nil))
      (dotimes (i (/ bits 8))
	(push (ldb (byte 8 (* i 8)) signed-immediate) res))
      res)))

(defun make-signed-byte (number)
  (ldb (byte 8 0) number))

(defun make-signed-dword (number)
  (ldb (byte 32 0) number))

(defun make-signed-qword (number)
  (ldb (byte 64 0) number))

(defun make-signed-immediate (number template)
  (ldb (byte (immediate-bits template) 0) number))

(defun little-endian-64bit (num)
  (let ((code nil))
    (dotimes (i 8)
      (push (ldb (byte 8 (* i 8)) num) code))
    (reverse code)))

(defun filter (list what)
  (let ((res nil))
    (dolist (l list)
      (when (not (equal l what))
	(push l res)))
    (reverse res)))

(defun two-complement (number bits)
  (ldb (byte bits 0) number))

(defun hex-opcodes (instructions)
  (mapcar (lambda (x) (format nil "#x~2,'0x" x))
	  instructions))

;;; we are checking all immediates as there are all signed so we can't assemble big unsigned integer as it is negative number 
(defun signed-number-type (number)
  (cond ((and (>= number *byte-min*)
	      (<=  number *byte-max*))
	 'byte)
	((and (>= number *word-min*)
	      (<= number *word-max*))
	 'word)
	((and (>= number *dword-min*)
	      (<= number *dword-max*))
	 'dword)
	((and (>= number *qword-min*)
	      (<= number *qword-max*))
	 'qword)
	(t (error  "Number is to large"))))

(defun opcode-d-bit (opcode)
  (logbitp 1 opcode))

(defun print-instruction (instruction)
  (let (bytes)
    (dolist (byte instruction)
      (when byte (push byte bytes)))
    (reverse (mapcar (lambda (b) (format nil "#x~2,'0x" b)) bytes))))

(defun make-byte-object ()
  (make-array 8 :initial-element nil))

(defun set-byte-index (byte-object index bit)
  (setf (aref byte-object index) bit))

(defun set-in-byte (byte-object start size what)
  (dotimes (i size)
    (print (list (+ i start) (- size i)))
    (setf (aref byte-object (+ i start)) (logbitp (- size i 1) what))))


(defun hash-keys (hash)
  (let (keys)
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k keys))
	     hash)
    keys))

(defun hash-values (hash)
  (let (vals)
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (push v vals))
	     hash)
    vals))
