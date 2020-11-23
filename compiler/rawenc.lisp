(in-package #:clcomp)

(defstruct vmem start current allocations data)
(defparameter *default-package* "CL")

(defun allocate-memory (start)
  (make-vmem :start start
	     :current start
	     :allocations (make-hash-table :test 'equal)
	     :data (make-hash-table)))

(defun get-maybe-allocated-object (vmem object)
  (gethash object (vmem-allocations vmem)))

(defun get-next-qword (vmem)
  (let ((qword (vmem-current vmem)))
    (setf (vmem-current vmem) (+ qword *word-size*))
    qword))

(defun get-next-n-qwords (vmem n)
  (let (adrs)
    (dotimes (i n)
      (push (get-next-qword vmem) adrs))
    (reverse adrs)))

(defun write-object (vmem address tagged-object)
  (setf (gethash address (vmem-data vmem)) tagged-object))

(defun add-allocated-obj (vmem object address)
  (setf (gethash object (vmem-allocations vmem)) address))

(defun allocate-cons (vmem conso)
  (let ((obj (get-maybe-allocated-object vmem conso)))
    (or obj
	(let* ((car (car conso))
	       (cdr (cdr conso))
	       (car-address (get-next-qword vmem))
	       (cdr-address (get-next-qword vmem))
	       (caradr (or (get-maybe-allocated-object vmem car)
			   (allocate-object vmem car)))
	       (cdradr (or (get-maybe-allocated-object vmem cdr)
			   (allocate-object vmem cdr))))
	  (write-object vmem car-address caradr)
	  (write-object vmem cdr-address cdradr)
	  (let ((object-addr (+ car-address *list-tag*)))
	    (add-allocated-obj vmem conso object-addr)
	    object-addr)))))

(defun allocate-string (vmem string)
  (let ((asym-addr (get-maybe-allocated-object vmem string)))
    (or asym-addr
	(let ((array-tag-addr (get-next-qword vmem))
	      (array-size-addr (get-next-qword vmem))
	      (array-type-addr (get-next-qword vmem))
	      (array-elem-type-addr (get-next-qword vmem))
	      (chars-addrs (get-next-n-qwords vmem (length string)))
	      (array-type (list 'simple-array 'character
				(list (length string)))))
	  (add-allocated-obj vmem string (+ array-tag-addr *pointer-tag*))
	  (write-object vmem array-tag-addr (get-extended-tag 'string))
	  (write-object vmem array-size-addr (ash (length string) *tag-size*))
	  (write-object vmem array-type-addr (or
					      (get-maybe-allocated-object vmem array-type)
					      (allocate-object vmem array-type)))
	  (write-object vmem array-elem-type-addr (or
						   (get-maybe-allocated-object vmem 'character)
						   (allocate-object vmem 'character)))
	  (dotimes (i (length string))
	    (let* ((char-addr (nth i chars-addrs))
		   (char-code (char-code (char string i))))
	      (write-object vmem char-addr (+ (ash char-code *tag-size*)
					      *char-tag*))))
	  (+ array-tag-addr *pointer-tag*)))))

(defun allocate-symbol (vmem sym)
  (let ((esym (get-maybe-allocated-object vmem sym)))
    (or esym
	(let* ((symbol-name (symbol-name sym))
	       (symbol-name-addr (get-next-qword vmem))
	       (symbol-value-addr (get-next-qword vmem))
	       (symbol-function-addr (get-next-qword vmem))
	       (symbol-plist-addr (get-next-qword vmem))
	       (symbol-package-addr (get-next-qword vmem))
	       (_ (add-allocated-obj vmem sym (+ symbol-name-addr *symbol-tag*)))
	       (allocated-sname-addr (or (get-maybe-allocated-object vmem symbol-name)
					 (allocate-string vmem symbol-name)))
	       (allocated-package-name-addr (or (get-maybe-allocated-object vmem *default-package*)
						(allocate-string vmem *default-package*))))
	  (declare (ignore _))
	  (add-allocated-obj vmem sym (+ symbol-name-addr *symbol-tag*))
	  (write-object vmem symbol-name-addr allocated-sname-addr)
	  (write-object vmem symbol-value-addr *nil*)
	  (write-object vmem symbol-function-addr *nil*)
	  (write-object vmem symbol-plist-addr *nil*)
	  (write-object vmem symbol-package-addr allocated-package-name-addr)
	  (+ symbol-name-addr *symbol-tag*)))))

(defun allocate-null (vmem)
  (declare (ignore vmem))
  *nil*)

(defun allocate-number (vmem number)
  (declare (ignore vmem))
  (+ (ash number *tag-size*) *fixnum-tag*))

(defun allocate-object (vmem object)
  (etypecase object
    (null (allocate-null vmem))
    (symbol (allocate-symbol vmem object))
    (string (allocate-string vmem object))
    (cons (allocate-cons vmem object))
    (number (allocate-number vmem object))))


(defun dump-data (vmem)
  (let ((sorted-keys (sort (hash-keys (vmem-data vmem))
			   (lambda (k1 k2)
			     (< k1 k2)))))
    (let (data)
      (dolist (k sorted-keys)
	(push (gethash k (vmem-data vmem)) data))
      (reverse data))))

