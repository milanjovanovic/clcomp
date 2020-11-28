(in-package #:clcomp)
(declaim (optimize (speed 0) (debug 3)))

(defparameter *runtime-heap-start* #x200000020)
(defparameter *compilation-start-address* *runtime-heap-start*)
(defparameter *rt-funs* (make-hash-table))


(defstruct compilation units)
(defparameter *current-compilation* (make-compilation :units nil))

(defparameter *load-time-fixups* nil)

(defparameter *start-address* *runtime-heap-start*)

(defparameter *clcomp-home* (namestring (ql:where-is-system "clcomp")))

(defparameter *compilation-unit-local-rips* nil)
(defparameter *compilation-unit-local-start-address* 0)
(defparameter *compile-component-start-address* 0)
(defparameter *compilation-unit-main-offset* 0)

(defparameter *vmem* nil)

(defparameter *do-break* nil)


(defun make-eval-fixup-pair (funcall-object fixup-address)
  (cons funcall-object fixup-address))

(defun add-fixup (fixup)
  (declare (optimize (debug 3)))
  (push fixup *load-time-fixups*))

(defun maybe-rt-%defun (name compilation-unit)
  ;; code loading will increase heap pointer
  (setf (compilation-unit-start compilation-unit) *compilation-start-address*)
  (when name
    (setf (gethash name *rt-funs*) (+ *compilation-start-address*
				      (compile-component-start (compilation-unit-compile-component compilation-unit)))))
  (setf *compilation-start-address* (+ *compilation-start-address* (get-compilation-unit-code-size compilation-unit))))


;; SYMBOLS that solves MAKE-SYMBOL and MAKE-STRING circularity
(defun process-bootstrap-data ()
  (let ((vmem (create-bootstrap-data)))
    (setf *compilation-start-address*
	  (+ *compilation-start-address* (* 8 (length (dump-data vmem)))))
    (setf *vmem* vmem)))

(defun dump-bootstrap-data (stream)
  (dolist (qword (dump-data *vmem*))
    (dolist (c (little-endian-64bit qword))
      (write-byte c stream))))

(defun make-bootstrap-data ()
  (let ((data nil))
    (dolist (qword (dump-data *vmem*))
      (push (little-endian-64bit qword) data))
    (reverse data)))

;; FIXME
;; for now we are only resolving FUNCTION calls
(defun resolve-fixup (fixup unit-code-buffer)
  (let* ((name (fun-rip-relative-name (rip-location-rip fixup)))
	 (offset (rip-location-byte-offset fixup))
	 (fun-address (rt-get-fun-address name)))
    (unless fun-address
      (error (format nil "Unknown fun ~a" name)))
    (when *debug*
      (format t "Name: ~a, Address: ~x~%" name fun-address))
    (let ((bytes (little-endian-64bit fun-address))
	  (index offset))
      (dolist (byte bytes)
	(setf (aref unit-code-buffer index) byte)
	(incf index)))))

(defun resolve-local-fixup (fixup unit-code-buffer)
  (let* ((name (component-rip-relative-name (rip-location-rip fixup)))
	 (offset (rip-location-byte-offset fixup))
	 (address (cdr (assoc name *compilation-unit-local-rips*))))
    (unless address
      (error (format nil "Unknown local RIP ~a" name)))
    (when *debug*
      (format t "Name: ~a, RIP Address: ~x~%" name address))
    (let ((bytes (little-endian-64bit address))
	  (index offset))
      (dolist (byte bytes)
	(setf (aref unit-code-buffer index) byte)
	(incf index)))))

(defun resolve-eval-load-compile-fixup (fixup)
  (declare (optimize (debug 3)))
  (let* ((name (fixup-rip-relative-name (rip-location-rip fixup)))
	 (offset (rip-location-byte-offset fixup))
	 (address (cdr (assoc name *compilation-unit-local-rips*))))
    (unless address
      (error (format nil "Unknown local RIP ~a" name)))
    (when *debug*
      (format t "Name: ~a, EVAL RIP Address: ~x~%" name address))
    (add-fixup  
     (make-eval-fixup-pair address (+ offset *compilation-unit-local-start-address*)))))

(defun resolve-compile-time-constant-fixup (fixup unit-code-buffer)
  (let* ((name (fixup-rip-relative-constant-name (rip-location-rip fixup)))
	 (offset (rip-location-byte-offset fixup))
	 (address (gethash (fixup-rip-relative-constant-form (rip-location-rip fixup))
			   (vmem-allocations *vmem*)))) ;; when getting vmem alloocarion we should look ar syjbol packkage
    (unless address
      (error (format nil "Unknown local RIP ~a" name)))
    (when *debug*
      (format t "Name: ~a, EVAL RIP Address: ~x~%" name address))
    (let ((bytes (little-endian-64bit address))
	  (index offset))
      (dolist (byte bytes)
	(setf (aref unit-code-buffer index) byte)
	(incf index)))))

(defun resolve-eval-at-load-time-fixup (compilation-unit)
  (let ((comp-component (compilation-unit-compile-component compilation-unit)))
    (add-fixup (cons (+ (compile-component-start comp-component)
		   (compilation-unit-start compilation-unit))
		0))))

(defun resolve-fixups (unit)
  (declare (optimize (debug 3)))
  (let ((unit-code (get-compilation-unit-code-buffer unit))
	(fixups (compilation-unit-fixups unit))
	(eval-at-load (compilation-unit-eval-at-load unit)))
    (if (listp fixups)
	(dolist (fixup fixups)
	  (cond ((fun-rip-relative-p (rip-location-rip fixup))
		 (resolve-fixup fixup unit-code))
		((component-rip-relative-p (rip-location-rip fixup))
		 (resolve-local-fixup fixup unit-code))
		((fixup-rip-relative-p (rip-location-rip fixup) )
		 (resolve-eval-load-compile-fixup fixup))
		((fixup-rip-relative-constant-p (rip-location-rip fixup))
		 (resolve-compile-time-constant-fixup fixup unit-code))))
	(resolve-fixup fixups unit-code))
    (when eval-at-load
      (resolve-eval-at-load-time-fixup unit))
    unit-code))

(defun print-hex-code (buffers)
  (with-output-to-string (s)
    (dolist (buffer (reverse buffers))
      (format s "~a" (apply #'concatenate 'string (mapcar #'byte-hex (coerce buffer 'list)))))
    s))

(defun process-compile-component (start-compile-component)
  (dolist (subcomp (reverse (flatten-comps (compile-component-subcomps start-compile-component)
					   #'identity)))
    (let ((rip (car subcomp))
	  (compile-component (cdr subcomp)))
      (push (cons (rip-relative-location-location rip) *compile-component-start-address*) *compilation-unit-local-rips*)
      (incf *compile-component-start-address*
	    (compile-component-code-size compile-component))
      (incf *compilation-unit-main-offset*
	    (compile-component-code-size compile-component)))))

(defun write-compilation-unit-code (comp-unit file-stream)
  (let ((*compilation-unit-local-rips* nil)
	(*compilation-unit-local-start-address* (compilation-unit-start comp-unit))
	(*compile-component-start-address* (compilation-unit-start comp-unit)))
    (process-compile-component (compilation-unit-compile-component comp-unit))
    (let ((code-buffer (resolve-fixups comp-unit)))
      (write-sequence code-buffer file-stream))))

(defun get-compilation-unit-code (comp-unit)
  (let ((*compilation-unit-local-rips* nil)
	(*compilation-unit-local-start-address* (compilation-unit-start comp-unit))
	(*compile-component-start-address* (compilation-unit-start comp-unit)))
    (process-compile-component (compilation-unit-compile-component comp-unit))
    (resolve-fixups comp-unit)))

(defun write-start-address (stream)
  (dolist (c (immediate-as-byte-list *start-address* :imm64))
    (write-byte c stream)))

(defun create-bootstrap-data ()
  (let ((vmem (allocate-memory *runtime-heap-start*)))
    (dolist (o *bootstrap-symbols*)
      (allocate-object vmem o))
    vmem))

(defun rt-dump-binary (file)
  (declare (optimize (debug 3)))
  (with-open-file (f file :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-start-address f)
    (dump-bootstrap-data f)
    (do*
     ((comps (reverse (compilation-units *current-compilation*)) (cdr comps))
      (comp (car comps) (car comps)))
     ((null comps) nil)
      (let ((*do-break* (when (null (cdr comps)) t)))
	(write-compilation-unit-code comp f)))))

(defun make-compilation-binary-data ()
  (let ((data))
    (do*
     ((comps (reverse (compilation-units *current-compilation*)) (cdr comps))
      (comp (car comps) (car comps)))
     ((null comps) nil)
      (let ((*do-break* (when (null (cdr comps)) t)))
	(push (get-compilation-unit-code comp) data)))
    (reverse data)))

(defun rt-dump-fixups (file)
  (declare (optimize (debug 3)))
  (with-open-file (f file :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (dolist (fixup (reverse *load-time-fixups*))
      (write-sequence (make-array *word-size*
				  :initial-contents (little-endian-64bit (car fixup)))
		      f)
      (write-sequence (make-array *word-size*
				  :initial-contents (little-endian-64bit (cdr fixup)))
		      f))))

(defun make-fixups-code-buffers-list ()
  (let ((data-buffers-list nil))
    (dolist (fixup (reverse *load-time-fixups*))
      (push (make-array *word-size*
			:initial-contents (little-endian-64bit (car fixup)))
	    data-buffers-list)
      (push (make-array *word-size*
			:initial-contents (little-endian-64bit (cdr fixup)))
	    data-buffers-list))
    (reverse data-buffers-list)))

(defun rt-add-to-compilation (compilation-unit)
  (setf (compilation-units *current-compilation*)
	(cons compilation-unit (compilation-units *current-compilation*))))

(defun rt-get-fun-address (name)
  (gethash name *rt-funs*))

(defun set-start-address (entry-compilation-unit)
  (setf *start-address* (+ (- *compilation-start-address*
			      (get-compilation-unit-code-size entry-compilation-unit))
			   (compile-component-start (compilation-unit-compile-component entry-compilation-unit)))))

(defun rt-reset ()
  (setf *current-compilation* (make-compilation :units nil))
  (setf *compilation-start-address* *runtime-heap-start*)
  (setf *start-address* *runtime-heap-start*)
  (setf *rt-funs* (make-hash-table)))

(defun test-compile-and-dump (form)
  (let ((*debug* nil)
	(*load-time-fixups* nil))
    (rt-reset)
    (process-bootstrap-data)
    ;; (clcomp-compile-file (format nil "~a/code/global.lisp" *clcomp-home*))
    ;; (clcomp-compile-file (format nil "~a/code/base.lisp" *clcomp-home*))
    ;; (clcomp-compile-file (format nil "~a/code/cons.lisp" *clcomp-home*))
    ;; (clcomp-compile-file (format nil "~a/code/array.lisp" *clcomp-home*))
    ;; (clcomp-compile-file (format nil "~a/code/seq.lisp" *clcomp-home*))
    ;; (clcomp-compile-file (format nil "~a/code/arith.lisp" *clcomp-home*))
    ;; (clcomp-compile-file (format nil "~a/code/call.lisp" *clcomp-home*))
    ;; (clcomp-compile-file (format nil "~a/code/symbol.lisp" *clcomp-home*))
    ;; (clcomp-compile-file (format nil "~a/code/char.lisp" *clcomp-home*))
    ;; (clcomp-compile-file (format nil "~a/code/tests.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/temp.lisp" *clcomp-home*))
    (set-start-address (clcomp-compile nil form))
    (maphash (lambda (k v)
	       (format t "~a -> ~x~%" k v))
	     *rt-funs*)
    (rt-dump-binary "/Users/milan/projects/clcomp.github/runtime/core")
    (rt-dump-fixups "/Users/milan/projects/clcomp.github/runtime/fixups.core")))

(defun compile-and-dump (form)
  (declare (optimize (debug 3)))
  (let ((*debug* nil)
	(*load-time-fixups* nil))
    (rt-reset)
    (process-bootstrap-data)
    (clcomp-compile-file (format nil "~a/code/global.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/base.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/cons.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/array.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/seq.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/arith.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/call.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/ffi.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/symbol.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/char.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/struct.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/io.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/tests.lisp" *clcomp-home*))
    (set-start-address (clcomp-compile nil form))
    (maphash (lambda (k v)
	       (format t "~a -> ~x~%" k v))
	     *rt-funs*)
    (let* ((bootstrap-data-list (make-bootstrap-data))
	   (code-buffers-list (make-compilation-binary-data))
	   (fixups-buffers-list (make-fixups-code-buffers-list))
	   (fixups-size (* *word-size* (length fixups-buffers-list))))
      (with-open-file (f "/Users/milan/projects/clcomp.github/runtime/core" :direction :output
									    :if-exists :supersede
									    :element-type '(unsigned-byte 8))

	(dolist (c (immediate-as-byte-list fixups-size :imm64))
	  (write-byte c f))
	(dolist (fixup-data-buffer fixups-buffers-list)
	  (write-sequence fixup-data-buffer f))
	(write-start-address f)
	(dolist (bootstrap-data bootstrap-data-list)
	  (write-sequence bootstrap-data f))
	(dolist (code-buffer code-buffers-list)
	  (write-sequence  code-buffer f))))))

