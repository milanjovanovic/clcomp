(in-package #:clcomp)
(declaim (optimize (speed 0) (debug 3) (safety 3)))


(defparameter *fun-address-reg* :RAX)
(defparameter *return-value-reg* :RBX)
(defparameter *fun-number-of-arguments-reg* :RCX) ;; we can use this one as scratch register since it's only used on fun entry
(defparameter *closure-env-reg* :RSI)
(defparameter *heap-header-reg* :R15)
(defparameter *fun-arguments-regs* '(:RDX :RDI :R8 :R9))
(defparameter *scratch-regs* '(:R10 :R11))
(defparameter *preserved-regs* '(:R12 :R13 :R14))


(defparameter *segment-instructions* nil)

(defstruct translator registers locations code)

(defun try-constant-component-blocks-phase (constants)
  (dolist (constant constants)
    (when (eq (type-of (cdr constant)) 'ir-component)
      (component-blocks-phase (cdr constant)))))

(defun component-blocks-phase (ir-component)
  (let ((blocks (make-blocks (ir-component-code ir-component))))
    (setf (ir-component-code-blocks ir-component)
	  (fill-used-locations (remove-dead-blocks (connect-blocks blocks))))
    (let ((all-blocks (ir-component-code-blocks ir-component)))
      (dolist (blok all-blocks)
	(setf (ir-block-ir blok) (remove-redundant-load-patterns blok all-blocks)))
      (dolist (blok all-blocks)
	(setf (ir-block-used-locations blok) nil)))
    (try-constant-component-blocks-phase (ir-component-constants ir-component))
    (setf (ir-component-code ir-component) nil)
    ir-component))


;;;;;;;;;;;;;;;;;;;;;;
;;; ir to assembly
;;;
;;; ir mnemonics :
;;; LAMBDA-ENTRY
;;; ARG-CHECK
;;; RECEIVE-PARAM
;;; PARAMS-COUNT
;;; LOAD-PARAM
;;; LOAD-RETURN
;;; LOAD
;;; IF
;;; LABEL
;;; GO


(defun emit-ir-assembly (translator asm)
  (setf (translator-code translator)
	(nconc (translator-code translator) asm)))

(defun calle-save-registers ()
  (let (assembly)
    (dolist (reg *preserved-regs*)
      (push (list :push reg) assembly))
    assembly))

(defun translate-lambda-entry (translator)
  (emit-ir-assembly translator
		    (list
		     (list :push :RBP)
		     (list :mov :RSP :RBP)))
  (emit-ir-assembly translator (calle-save-registers)))

(defun translate-arg-check (ir translator)
  (let ((arg-count (second ir)))
    (declare (ignore arg-count))
    (emit-ir-assembly translator
		      "FIXME")))

(defun translate-params-count (translator ir)
  (let ((arguments-count (second ir)))
    (emit-ir-assembly translator
		      (list
		       (list :mov *fun-number-of-arguments-reg* arguments-count)))
    (when (> arguments-count (length *fun-arguments-regs*))
      (emit-ir-assembly translator
			(list (list :sub :RSP (* *word-size* (- arguments-count (length *fun-arguments-regs*)))))))))

(defun translate-receive-param (ir translator)
  ;; move arguments to scratch registers
  (declare (ignore ir translator)))

(defun translate-load (ir translator)
  (declare (ignore ir translator)))

;;; FIXME, translate-load and translate-load-param can be one function
(defun translate-load-param (ir translator)
  (declare (ignore ir translator)))

(defun translate-load-return (ir translator)
  (declare (ignore ir translator)))

#+nil
 (defun translate-if (ir translator)
   (let (())))

(defun translate-go (ir translator)
  (emit-ir-assembly translator
		    (list
		     (list :jmp-fixup :jmp (second ir)))))

(defun translate-label (ir translator)
  (emit-ir-assembly translator
		    (list
		     (list :label (second ir)))))

(defun to-asm (ir translator)
  (let ((mnemonic (first ir)))
    (case mnemonic
      (lambda-entry (translate-lambda-entry translator))
      (arg-check (translate-arg-check ir translator))
      (params-count (translate-params-count ir translator))
      (receive-param (translate-receive-param ir translator))
      (load (translate-load ir translator))
      (load-param (translate-load-param ir translator))
      (load-return (translate-load-return ir translator)) ;;; FIXME, need to set RCX to nunmer of arguments
      (if (translate-if ir translator))
      (go (translate-go ir translator))
      (label (translate-label ir translator)))))



;;; linear scan register allocation

(defstruct intervals map)
(defun get-sorted-intervals (intervals)
  (let (res)
    (maphash (lambda (k v)
	       (push (list k v) res))
	     (intervals-map intervals))
    (sort res #'< :key (lambda (e) (first (second e))))))

(defun add-interval (intervals location interval-num)
  (let* ((var-map (intervals-map intervals))
	 (var-interval (gethash (location-symbol location) var-map)))
    (if var-interval
	(setf (cdr var-interval) interval-num)
	(setf (gethash (location-symbol location) var-map) (cons interval-num interval-num)))))

(defun add-to-interval (intervals location index)
  (case (type-of location)
    ((var-location tmp-location) (add-interval intervals location index))))

(defun add-from-interval (intervals location index)
  (case (type-of location)
    ((var-location tmp-location ret-location) (add-interval intervals location index))))

(defun create-ir-intervals (ir-component)
  (let ((intervals (make-intervals :map (make-hash-table)))
	(index 1))
    (dolist (blok (ir-component-code-blocks ir-component))
      (dolist (ir (ir-block-ir blok))
	(let ((mnemonic (first ir)))
	  (case mnemonic
	    (receive-param (add-from-interval intervals (second ir) index))
	    (load-param (add-from-interval intervals (get-load-from ir) index))
	    (load (add-from-interval intervals (get-load-from ir) index)
	     (add-to-interval intervals (get-load-to ir) index))
	    (load-return (add-from-interval intervals (get-load-from ir) index))
	    (if (add-from-interval intervals (second ir) index))
	    (otherwise nil)))
	(incf index)))
    intervals))

(defstruct allocation active map stack reg registers-available)
(defstruct reg-storage register)
(defstruct stack-storage offset)

(defun interval-symbol (interval)
  (first interval))
(defun interval-start (interval)
  (car (second interval)))
(defun interval-end (interval)
  (cdr (second interval)))

(defun expire-old-intervals (allocation start)
  (let (new-active)
    (dolist (interval (allocation-active allocation))
      (cond ((< (interval-end interval) start)
	     (decf (allocation-reg allocation)))
	    (t (push interval new-active))))
    (setf (allocation-active allocation)
	  (sort new-active #'< :key (lambda (int) (interval-end int))))
    allocation))

(defun add-live-interval (allocation interval)
  (let ((new-active (cons interval
			  (allocation-active allocation)))
	(int-sym (interval-symbol interval)))
    (setf (allocation-active allocation)
	  (sort new-active #'< :key (lambda (int) (interval-end int))))
    (setf (gethash int-sym (allocation-map allocation)) (make-reg-storage :register (allocation-reg allocation)))
    (incf (allocation-reg allocation))))

(defun spill-interval (allocation interval)
  (let* ((last-interval (car (last (allocation-active allocation))))
	 (greater (>= (interval-end interval) (interval-end last-interval))))
    (cond (greater
	   (setf (gethash (interval-symbol interval) (allocation-map allocation))
		 (make-stack-storage :offset (allocation-stack allocation)))
	   (incf (allocation-stack allocation)))
	  (t
	   (setf (gethash (interval-symbol last-interval) (allocation-map allocation))
		 (make-stack-storage :offset (allocation-stack allocation)))
	   (incf (allocation-stack allocation))
	   (let ((new-active (cons interval (butlast (allocation-active allocation)))))
	     (setf (allocation-active allocation)
		   (sort new-active #'< :key (lambda (int) (interval-end int)))))))))

(defun allocate-storage (intervals)
  (let ((allocation (make-allocation :map (make-hash-table) :stack 0 :reg 0 :registers-available (length *preserved-regs*))))
    (dolist (interval (get-sorted-intervals intervals))
      (expire-old-intervals allocation (interval-start interval))
      (cond ((< (length (allocation-active allocation)) (allocation-registers-available allocation))
	     (add-live-interval allocation interval))
	    (t (spill-interval allocation interval))))
    allocation))

(defun translate-ir (ir-code)
  (let ((translator (make-translator :registers (make-hash-table)
				     :locations (make-hash-table))))
    (dolist (ir ir-code)
      (to-asm ir translator))))

(defun clcomp-compile (exp)
  (let* ((ir-component (make-ir (create-node (expand exp)))))
    (component-blocks-phase ir-component)
    (let ((ir-code (ir-component-code ir-component)))
      (translate-ir ir-code)
      ir-component)))
