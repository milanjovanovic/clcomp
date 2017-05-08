(in-package #:clcomp)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defparameter *fun-address-reg* :RAX)
(defparameter *return-value-reg* :RBX)
(defparameter *fun-number-of-arguments-reg* :RCX)
(defparameter *closure-env-reg* :RSI)
(defparameter *heap-header-reg* :R15)
(defparameter *fun-arguments-regs* '(:RDX :RDI :R8 :R9))
(defparameter *scratch-regs* '(:R10 :R11))
(defparameter *tmp-reg* :R10)
(defparameter *preserved-regs* '(:R12 :R13 :R14))


(defparameter *debug* t)
;;;;;;;;;;;;;;;;;;;;;;
;;; ir to assembly
;;;
;;; ir mnemonics :
;;; LAMBDA-ENTRY
;;; ARG-CHECK
;;; RECEIVE-PARAM
;;; PARAMS-COUNT
;;; LOAD-PARAM
;;; LOAD-CALL
;;; LOAD
;;; IF
;;; LABEL
;;; GO
;;; LAMBDA-EXIT

(defun emit-ir-assembly (translator asm)
  (when *debug*
    (dolist (a asm)
      (print a)))
  (setf (translator-code translator)
	(nconc (translator-code translator) asm)))

(defun asm-storage-move (storage1 storage2)
  (let ((s1-type (type-of storage1))
	(s2-type (type-of storage2)))
    (cond ((and (or (eq s1-type 'stack-storage)
		    (eq s1-type 'memory-storage))
		(or (eq s2-type 'stack-storage)
		    (eq s2-type 'memory-storage)
		    (eq s2-type 'constant-storage)))
	   (list
	    (list :mov *tmp-reg* (storage-operand storage2))
	    (list :mov (storage-operand storage1) *tmp-reg*)))
	  (t
	   (list
	    (list :mov (storage-operand storage1) (storage-operand storage2)))))))

(defun asm-move (loc1 loc2 allocation)
  (asm-storage-move (get-allocation-storage loc1 allocation)
		    (get-allocation-storage loc2 allocation)))

(defun calle-save-registers ()
  (let (assembly)
    (dolist (reg *preserved-regs*)
      (push (list :push reg) assembly))
    assembly))

(defun allocate-stack-space (allocation)
  (list
   (list :sub :RSP (* *word-size* (allocation-stack allocation)))))

(defun translate-lambda-entry (translator allocation)
  (emit-ir-assembly translator
		    (list
		     (list :push :RBP)
		     (list :mov :RBP :RSP)))
  (emit-ir-assembly translator (calle-save-registers))
  (emit-ir-assembly translator (allocate-stack-space allocation)))

(defun translate-arg-check (ir translator)
  (let ((arg-count (second ir)))
    (declare (ignore arg-count))
    (emit-ir-assembly translator
		      (list
		       (list :cmp *fun-number-of-arguments-reg* (fixnumize-positive (second ir)))
		       (list :jmp-fixum :jmp :wrong-arg-count-label)))))

(defun translate-params-count (ir translator)
  (let ((arguments-count (second ir)))
    (emit-ir-assembly translator
		      (list
		       (list :mov *fun-number-of-arguments-reg* (fixnumize-positive arguments-count))))
    (when (> arguments-count (length *fun-arguments-regs*))
      (emit-ir-assembly translator
			(list (list :sub :RSP (* *word-size* (- arguments-count (length *fun-arguments-regs*)))))))))

(defun translate-receive-param (param-location param-index translator allocation)
  (let ((storage (get-allocation-storage param-location allocation)))
    (when storage
      (emit-ir-assembly translator
			(asm-storage-move storage (make-receive-param-storage param-index))))))

(defun translate-load (ir translator allocation)
  (emit-ir-assembly translator
		    (asm-move (get-load-to ir) (get-load-from ir) allocation)))

(defun translate-load-param (ir translator allocation)
  (emit-ir-assembly translator
		    (asm-move (get-load-to ir) (get-load-from ir) allocation)))

(defun translate-load-call (ir translator allocation)
  (emit-ir-assembly translator
		    (list
		     (list :mov *fun-address-reg* :fixup (fourth ir))
		     (list :call (@ *fun-address-reg*))))
  (let ((storage (get-allocation-storage (second ir) allocation)))
    (when storage
      (emit-ir-assembly translator
			(list
			 (list :mov (storage-operand storage) *return-value-reg*))))))

(defun translate-if (ir translator)
  ;; FIXME
  (list ir))

(defun translate-go (ir translator)
  (emit-ir-assembly translator
		    (list
		     (list :jmp-fixup :jmp (second ir)))))

(defun translate-label (ir translator)
  (emit-ir-assembly translator
		    (list
		     (list :label (second ir)))))

(defun translate-lambda-exit (translator)
  (emit-ir-assembly translator
		    (list
		     (list :ret)
		     (list :label :wrong-arg-count-label)
		     (list :byte "TRAP"))))

(defun to-asm (ir translator allocation)
  (let ((mnemonic (first ir)))
    (when *debug*
      (progn
	(format t "~%~%---------------------------------------------------")
	(format t "~%~%")
	(print ir)
	(format t "~%")))
    (case mnemonic
      (lambda-entry (translate-lambda-entry translator allocation))
      (arg-check (translate-arg-check ir translator))
      (params-count (translate-params-count ir translator))
      (receive-param (translate-receive-param (second ir) (third ir) translator allocation))
      (load (translate-load ir translator allocation))
      (load-param (translate-load-param ir translator allocation))
      (load-call (translate-load-call ir translator allocation)) ;;; FIXME, need to set RCX to nunmer of arguments
      (if (translate-if ir translator))
      (go (translate-go ir translator))
      (label (translate-label ir translator))
      (lambda-exit (translate-lambda-exit translator)))))

(defun component-to-asm (ir-component allocation)
  (let ((translator (make-translator)))
    (dolist (blok (ir-component-code-blocks ir-component))
      (dolist (ir (ir-block-ir blok))
	(to-asm ir translator allocation)))
    translator))



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
	    (receive-param (add-from-interval intervals (second ir) 1))
	    (load-param (add-from-interval intervals (get-load-from ir) index))
	    (load (add-from-interval intervals (get-load-from ir) index)
	     (add-to-interval intervals (get-load-to ir) index))
	    (load-call (add-from-interval intervals (get-load-from ir) index))
	    (if (add-from-interval intervals (second ir) index))
	    (otherwise nil)))
	(incf index)))
    intervals))

(defstruct allocation active map stack reg registers-available)
(defstruct reg-storage register)
;;; FIXME, no need for stack-storage, should be memory-storage with RBP as base
(defstruct stack-storage offset)
(defstruct memory-storage base offset)
(defstruct constant-storage constant)

(defun storage-operand (storage)
  (case (type-of storage)
    (constant-storage (constant-storage-constant storage))
    (reg-storage (reg-storage-register storage))
    (stack-storage (@ :RBP (- (* *word-size* (stack-storage-offset storage)))))
    (memory-storage (@ (memory-storage-base storage) (memory-storage-offset storage)))))

(defun make-receive-param-storage (param-number)
  (if (> param-number (length *fun-arguments-regs*))
      ;; we already did push RBP to stack and with return addres that's 2 slots + all calle-saved registers
      (make-memory-storage :base :RBP :offset (* *word-size* (+ 2
								(length *preserved-regs*)
								(- param-number (length *fun-arguments-regs*) 1))))
      (make-reg-storage :register (nth (- param-number 1) *fun-arguments-regs*))))

(defun get-param-storage (param-number)
  ;; params numbers starts from 1
  (if (> param-number (length *fun-arguments-regs*))
      (make-memory-storage :base :RSP :offset (- param-number (length *fun-arguments-regs*)))
      (make-reg-storage :register (nth (- param-number 1) *fun-arguments-regs*))))

(defun get-allocation-storage (location allocation)
  (let ((location-type (type-of location)))
    (case location-type
      (constant-storage (make-constant-storage :constant (immediate-constant-constant location)))
      (param-location (get-param-storage (param-location-param-number location)))
      (ret-location (or (gethash (location-symbol location) (allocation-map allocation))
			(make-reg-storage :register *return-value-reg*)))
      ((var-location tmp-location) (gethash (location-symbol location) (allocation-map allocation)))
      (otherwise (error "Unknown location type")))))

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
    (setf (gethash int-sym (allocation-map allocation)) (make-reg-storage :register (nth (allocation-reg allocation)
											 *preserved-regs*)))
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


#+nil
(defun translate-ir ()
  (let ((translator (make-translator :registers (make-hash-table)
				     :locations (make-hash-table))))
    (dolist (ir ir-code)
      )))

(defun clcomp-compile (exp)
  (let* ((ir-component (make-ir (create-node (expand exp)))))
    (component-blocks-phase ir-component)
    (let ((ir-code (ir-component-code ir-component)))
      ir-component)))
