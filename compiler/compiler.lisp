(in-package #:clcomp)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defparameter *base-pointer-reg* :RBP)
(defparameter *stack-pointer-reg* :RSP)
(defparameter *fun-address-reg* :RAX)
(defparameter *return-value-reg* :RBX)
(defparameter *fun-number-of-arguments-reg* :RCX)
(defparameter *closure-env-reg* :RSI)
(defparameter *heap-header-reg* :R15)
(defparameter *fun-arguments-regs* '(:RDX :RDI :R8 :R9))
(defparameter *scratch-regs* '(:R10 :R11))
(defparameter *tmp-reg* :R10)
(defparameter *preserved-regs* '(:R12 :R13 :R14))


(defparameter *debug* nil)

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


;;;;  STACK
;;;; CSRN - calle saved registers
;;;; L1 - local variable
;;;; ARGN - function arguments with no register
;;;
;;; ARG6  RBP+24
;;; ARG5  RBP+16
;;; RET   RBP+8
;;; RBP   RBP
;;; CSR1    RBP-8
;;; CSR2    RBP-16
;;; CSR3    RBP-24
;;; L1   RBP-32
;;; L2   RBP-40
;;; L3   RBP-48
;;; L4   RBP-56

(defparameter *dummy-rip-value* (list #x90 #x90 #x90 #x90 #x90 #x90 #x90 #x90))

;;; FIXME, is this right sequence ??
(defparameter *error-trap* '((:byte #x0F) (:byte #x0B)))

(defun make-nops (count)
  (let (nops)
    (dotimes (i count)
      (push (list :byte #x90) nops))
    nops))

(defstruct translator registers locations code)

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

(defun asm-storage-move-from-rip-relative (storage1 rip-location)
  (let ((s1-type (type-of storage1)))
    (if (or (eq s1-type 'stack-storage)
	    (eq s1-type 'memory-storage))
	(list
	 (list :rip-relative-fixup :mov *tmp-reg* (list 'subcomp (rip-relative-location-location rip-location)))
	 (list :mov (storage-operand storage1) *tmp-reg*))
	(list
	 (list :rip-relative-fixup :mov (storage-operand storage1) (list 'subcomp (rip-relative-location-location rip-location)))))))

(defun asm-move (loc1 loc2 allocation)
  (if (eq 'rip-relative-location (type-of loc2))
      (asm-storage-move-from-rip-relative (get-allocation-storage loc1 allocation) loc2)
      (asm-storage-move (get-allocation-storage loc1 allocation)
			(get-allocation-storage loc2 allocation))))

(defun calle-save-registers ()
  (let (assembly)
    (push (list :sub :RSP 8) assembly)
    (dolist (reg *preserved-regs*)
      (push (list :push reg) assembly))
    assembly))

(defun calle-restore-registers ()
  (let (assembly)
    (dolist (reg (reverse *preserved-regs*))
      (push (list :pop reg) assembly))
    (push (list :add :RSP 8) assembly)
    assembly))

(defun maybe-allocate-stack-space (translator allocation)
  (when (> (allocation-stack allocation) 0)
    (emit-ir-assembly translator
		      (list
		       (list :sub *stack-pointer-reg*
			     (* *word-size* (allocation-stack allocation)))))))

(defun maybe-deallocate-stack-space (translator allocation)
  (when (> (allocation-stack allocation) 0)
    (emit-ir-assembly translator
		      (list
		       (list :add *stack-pointer-reg*
			     (* *word-size* (allocation-stack allocation)))))))

(defun translate-lambda-entry (translator allocation)
  (emit-ir-assembly translator
		    (list
		     (list :push *base-pointer-reg*)
		     (list :mov *base-pointer-reg* *stack-pointer-reg*)))
;;  #+nil ; better do caller save register
  (emit-ir-assembly translator (calle-save-registers))
  (maybe-allocate-stack-space translator allocation))

(defun translate-arg-check (ir translator)
  (let ((arg-count (second ir)))
    (declare (ignore arg-count))
    (emit-ir-assembly translator
		      (list
		       (list :cmp *fun-number-of-arguments-reg* (fixnumize (second ir)))
		       (list :jump-fixup :jne :wrong-arg-count-label)))))

(defun translate-params-count (ir translator)
  (let ((arguments-count (second ir)))
    (emit-ir-assembly translator
		      (list
		       (list :mov *fun-number-of-arguments-reg* (fixnumize arguments-count))))
    (when (> arguments-count (length *fun-arguments-regs*))
      (emit-ir-assembly translator
			(list (list :sub *stack-pointer-reg*
				    (* *word-size* (- arguments-count (length *fun-arguments-regs*)))))))))

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
		     (list :rip-relative-fixup :mov *fun-address-reg* (list 'function-address (fourth ir)))
		     (list :call (@ *fun-address-reg*))))
  (let ((arguments-count (nth 4 ir)))
    (when (> arguments-count (length *fun-arguments-regs*))
      (emit-ir-assembly translator
			(list (list :add *stack-pointer-reg*
				    (* *word-size* (- arguments-count (length *fun-arguments-regs*))))))))
  (let ((storage (get-allocation-storage (second ir) allocation)))
    (when (and storage
	       (when (typep storage 'reg-storage)
		 (not (eq (reg-storage-register storage) *return-value-reg*))))
      (emit-ir-assembly translator
			(list
			 (list :mov (storage-operand storage) *return-value-reg*))))))

(defun emit-vop-code (vop ir allocation translator)
  ;; we are using fun arguments registers for our VOP
  ;; allwaus use *RETURN-VALUE-REG* as return register
  ;; FIXME, is this OK ?
  (let (arguments-regs
	(usable-registers *fun-arguments-regs*)
	(ir-arguments (get-ir-vop-args ir)))
    (dolist (argument ir-arguments)
      (let ((arg-storage (get-allocation-storage argument allocation)))
	(etypecase arg-storage
	  (reg-storage
	   (push (reg-storage-register arg-storage) arguments-regs))
	  (stack-storage
	   (let ((register (first usable-registers)))
	     (emit-ir-assembly translator (asm-storage-move (make-reg-storage :register register)
							    arg-storage))
	     (push register arguments-regs))
	   (setf usable-registers (rest usable-registers))))))
    (emit-ir-assembly translator
		      (get-vop-code vop (cons *return-value-reg*  (reverse arguments-regs))))
    (emit-ir-assembly translator
		      (list
		       (list :mov (storage-operand (get-allocation-storage (third ir) allocation))
			     *return-value-reg*)))))

(defun translate-vop-call (ir translator allocation)
  (let ((vop-name (get-ir-vop-name ir))
	(ir-vop-args (get-ir-vop-args ir)))
    (if vop-name
	(let* ((vop (get-vop vop-name)))
	  (if vop
	      (let ((regs (length (vop-arguments vop))))
		(when (> regs (length *fun-arguments-regs*))
		  (error (format nil "Not enough regs for VOP ~a" ir)))
		(when (not (= (length ir-vop-args) regs))
		  (error (format nil "Wrong number of arguments for VOP in ~a" ir)))
		(emit-vop-code vop ir allocation translator))
	      (error (format nil "Unknown vop for ir ~a" ir))))
	(error (format nil "Unknown vop for ir ~a" ir)))))

(defun translate-if (ir translator allocation)
  (let* ((location (second ir))
	 (storage (get-allocation-storage location allocation))
	 (storage-type (type-of storage))
	 (test-storage storage))
    (when (or
	   (eq storage-type 'stack-storage)
	   (eq storage-type 'memory-storage))
      (emit-ir-assembly translator
			(list
			 (list :mov (make-reg-storage :register *tmp-reg*) storage)))
      (setf test-storage (make-reg-storage :register *tmp-reg*)))
    (emit-ir-assembly translator
		      (list
		       (list :cmp (storage-operand test-storage) *nil*)
		       (list :jump-fixup :jne (fourth ir))))))

(defun translate-go (ir translator)
  (emit-ir-assembly translator
		    (list
		     (list :jump-fixup :jmp (second ir)))))

(defun translate-label (ir translator)
  (emit-ir-assembly translator
		    (list
		     (list :label (second ir)))))

(defun translate-lambda-exit (translator allocation)
  (emit-ir-assembly translator (list (list :clc)))
  (maybe-deallocate-stack-space translator allocation)
  (emit-ir-assembly translator (calle-restore-registers))
  (emit-ir-assembly translator
		    (list
		     (list :pop *base-pointer-reg*)
		     (list :ret)
		     (list :label :wrong-arg-count-label)))
  (emit-ir-assembly translator
		    *error-trap*))

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
      (load-call (translate-load-call ir translator allocation))
      (vop (translate-vop-call ir translator allocation))
      (if (translate-if ir translator allocation))
      (go (translate-go ir translator))
      (label (translate-label ir translator))
      (lambda-exit (translate-lambda-exit translator allocation)))))

(defun translate-component (ir-component allocation)
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
  (typecase location
    ((or tmp-location var-location ret-location)
     (let* ((var-map (intervals-map intervals))
	    (var-interval (gethash (location-symbol location) var-map)))
       (if var-interval
	   (setf (cdr var-interval) interval-num)
	   (setf (gethash (location-symbol location) var-map) (cons interval-num interval-num)))))))

(defun create-ir-intervals (ir-component)
  (let ((intervals (make-intervals :map (make-hash-table)))
	(index 1))
    (dolist (blok (ir-component-code-blocks ir-component))
      (dolist (ir (ir-block-ir blok))
	(let ((mnemonic (first ir)))
	  (case mnemonic
	    (receive-param (add-interval intervals (second ir) 1))
	    (load-param (add-interval intervals (get-load-from ir) index))
	    (load (add-interval intervals (get-load-from ir) index)
	     (add-interval intervals (get-load-to ir) index))
	    (load-call
	     (add-interval intervals (second ir) index))
	    (if (add-interval intervals (second ir) index))
	    (vop
	     (add-interval intervals (third ir) index)
	     (mapcar (lambda (v)
		       (add-interval intervals v index))
		     (get-ir-vop-args ir)) )
	    (otherwise nil)))
	(incf index)))
    intervals))

(defstruct allocation active map stack registers-available)
(defstruct reg-storage register)
;;; FIXME, no need for stack-storage, should be memory-storage with RBP as base
(defstruct stack-storage offset)
(defstruct memory-storage base offset)
(defstruct constant-storage constant)

(defun storage-operand (storage)
  (case (type-of storage)
    (constant-storage (constant-storage-constant storage))
    (reg-storage (reg-storage-register storage))
    (stack-storage (@ *base-pointer-reg* nil nil (- (* *word-size* (+ (stack-storage-offset storage)
								      (length *preserved-regs*)
								      1)))))
    (memory-storage (@ (memory-storage-base storage) nil nil (memory-storage-offset storage)))))

(defun make-receive-param-storage (param-number)
  (if (> param-number (length *fun-arguments-regs*))
      ;; we already did push RBP to stack and with return addres that's 2 slots
      (make-memory-storage :base *base-pointer-reg* :offset (* *word-size* (+ 2 (- param-number (length *fun-arguments-regs*) 1))))
      (make-reg-storage :register (nth (- param-number 1) *fun-arguments-regs*))))

(defun get-param-storage (param-number)
  (let ((arg-reg-count (length *fun-arguments-regs*)))
    (if (> param-number arg-reg-count)
	(make-memory-storage :base *stack-pointer-reg* :offset (* *word-size* (- param-number arg-reg-count 1)))
	(make-reg-storage :register (nth (- param-number 1) *fun-arguments-regs*)))))

(defun get-allocation-storage (location allocation)
  (let ((location-type (type-of location)))
    (case location-type
      (immediate-constant (make-constant-storage :constant (immediate-constant-constant location)))
      (param-location (get-param-storage (param-location-param-number location)))
      (ret-location (or (gethash (location-symbol location) (allocation-map allocation))
			(make-reg-storage :register *return-value-reg*)))
      ((var-location tmp-location) (gethash (location-symbol location) (allocation-map allocation)))
      (rip-relative-location (make-memory-storage))
      (lambda-return-location (make-reg-storage :register *return-value-reg*))
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
	     (let ((interval-storage (gethash (interval-symbol interval) (allocation-map allocation))))
	       (push (reg-storage-register interval-storage) (allocation-registers-available allocation))))
	    (t (push interval new-active))))
    (setf (allocation-active allocation)
	  (sort new-active #'< :key (lambda (int) (interval-end int))))
    allocation))

(defun add-live-interval (allocation interval)
  (let ((new-active (cons interval
			  (allocation-active allocation)))
	(int-sym (interval-symbol interval))
	(register (pop (allocation-registers-available allocation))))
    (setf (allocation-active allocation)
	  (sort new-active #'< :key (lambda (int) (interval-end int))))
    (setf (gethash int-sym (allocation-map allocation)) (make-reg-storage :register register))
    allocation))

(defun spill-interval (allocation interval)
  (let* ((last-interval (car (last (allocation-active allocation))))
	 (greater (>= (interval-end interval) (interval-end last-interval))))
    (cond (greater
	   (setf (gethash (interval-symbol interval) (allocation-map allocation))
		 (make-stack-storage :offset (allocation-stack allocation)))
	   (incf (allocation-stack allocation)))
	  (t
	   (let ((last-interval-storage (gethash (interval-symbol last-interval)
						 (allocation-map allocation))))
	     (push (reg-storage-register last-interval-storage) (allocation-registers-available allocation))
	     (setf (gethash (interval-symbol last-interval) (allocation-map allocation))
		   (make-stack-storage :offset (allocation-stack allocation))))
	   (incf (allocation-stack allocation))
	   (setf (allocation-active allocation) (butlast (allocation-active allocation)))
	   (add-live-interval allocation interval)))))

(defun create-component-allocation (intervals)
  (let ((allocation (make-allocation :map (make-hash-table) :stack 0 :registers-available *preserved-regs*)))
    (dolist (interval (get-sorted-intervals intervals))
      (expire-old-intervals allocation (interval-start interval))
      (cond ((> (length (allocation-registers-available allocation)) 0)
	     (add-live-interval allocation interval))
	    (t (spill-interval allocation interval))))
    allocation))


(defstruct rip-location rip byte-offset)
(defstruct compile-component code prefix-code start code-size subcomps rips rip-offsets byte-code) 
(defstruct compilation-unit compile-component start fixups code)

(defun get-compilation-unit-code-buffer (compilation-unit)
  (let* ((code (compilation-unit-code compilation-unit))
	 (code-size (reduce (lambda (size inst)
			      (incf size (length inst)))
			    code :initial-value 0))
	 (buffer (make-array code-size :element-type '(unsigned-byte 8))))
    (let ((index 0))
      (dolist (inst code)
	(dolist (b inst)
	  (setf (aref buffer index) b)
	  (incf index)))
      buffer)))

(defun compile-component-pass-1 (ir-component)
  (let* ((intervals (create-ir-intervals ir-component))
	 (allocation (create-component-allocation intervals)))
    (make-compile-component :code (translator-code (translate-component ir-component allocation))
			    :subcomps (mapcar (lambda (subcomp)
						(cons (sub-component-name subcomp)
						      (compile-component-pass-1 (sub-component-component subcomp))))
					      (ir-component-sub-comps ir-component))
			    :rips (ir-component-rips ir-component))))

(defun try-component-sub-components-blocks-phase (sub-components)
  (dolist (s-component sub-components)
    (let ((component (sub-component-component s-component)))
      (when (eq (type-of component) 'ir-component)
	(component-blocks-phase component)))))

(defun component-blocks-phase (ir-component)
  (let ((blocks (make-blocks (ir-component-code ir-component))))
    (setf (ir-component-code-blocks ir-component)
	  (fill-used-locations (remove-dead-blocks (connect-blocks blocks))))
    (let ((all-blocks (ir-component-code-blocks ir-component)))
      (dolist (blok all-blocks)
	(setf (ir-block-ir blok) (remove-redundant-load-patterns blok all-blocks)))
      (dolist (blok all-blocks)
	(setf (ir-block-used-locations blok) nil)))
    (try-component-sub-components-blocks-phase (ir-component-sub-comps ir-component))
    (setf (ir-component-code ir-component) nil)
    ir-component))


;;; component assemble process

;;; adjust every compile-component to *allocation-size* borders 
(defun set-and-maybe-adjust-code-size (component)
  (let* ((prefix-code-size (code-size (compile-component-prefix-code component)))
	 (code-size (code-size (compile-component-byte-code component)))
	 (size (+ prefix-code-size code-size))
	 (mod (mod size *allocation-size*)))
    (if (> mod 0)
	(progn
	  (setf (compile-component-byte-code component)
		(append (compile-component-byte-code component)
			(assembly-pass-1 (make-nops (- *allocation-size* mod)))))
	  (setf (compile-component-code-size component) (+ size (- *allocation-size* mod))))
	(setf (compile-component-code-size component) size))))

(defun make-compile-unit-and-compile-pass-1 (ir-component)
  (make-compilation-unit :compile-component (compile-component-pass-1 ir-component)))

;; FIXME
;; remove zero jumps, remove
;; remove redudand loads -> (mov rbx, rcx; mov rcx, rbx)
;; find consecutive store to same location, leave only last -> (mov rax, 10 ; mov rax 20)
(defun peephole (assembly)
  assembly)

(defun assemble-component (component)
  (let ((main-code (assembly-pass-1 (peephole (resolve-assembly-jumps (compile-component-code component)))))
	(subcomps (compile-component-subcomps component))
 	(rips (compile-component-rips component)))
    (let ((pre-code nil)
	  (rip-offsets nil)
	  (start-offset (+ (length subcomps)
			   (length (remove-if (lambda (rip)
						(typep rip 'component-rip-relative))
					      rips)))))
      (when (> start-offset 0)
	(push (assemble-instruction (list :jmp (* *word-size* start-offset))) pre-code)
	(when (> (length subcomps) 0)
	  (dolist (subcomp subcomps)
	    (push *dummy-rip-value* pre-code)
	    (push (list (rip-relative-location-location (car subcomp)) start-offset) rip-offsets)
	    (decf start-offset)))
	;; non subcomponent rips
	(when (> (length rips) (length subcomps))
	  (dolist (rip-relative rips)
	    (when (not (typep rip-relative 'component-rip-relative))
	      (push *dummy-rip-value* pre-code)
	      (push (list (get-rip-relative-name rip-relative) start-offset) rip-offsets))
	    (decf start-offset))))
      (setf (compile-component-byte-code component)
	    main-code)
      (setf (compile-component-prefix-code component)
	    (reverse pre-code))
      (setf (compile-component-rip-offsets component)
	    rip-offsets)
      (resolve-component-rips component))))

(defun get-rip-offset (component rip-name)
  (second (assoc rip-name (compile-component-rip-offsets component))))

(defun resolve-component-rips (component)
  (let ((rip-offsets (compile-component-rip-offsets component)))
    (when rip-offsets
      (let ((code nil)
	    (current-size 0))
	(dolist (inst (compile-component-byte-code component))
	  (incf current-size (instruction-size inst))
	  (cond ((eq (first inst) :rip-relative-fixup)
		 (let* ((mnemonic (second inst))
			(to (third inst))
			(rip-offset-name (second (fourth inst)))
			(rip (list *instruction-pointer-register* nil nil (- (+ current-size
										(* *word-size* (get-rip-offset component rip-offset-name))))))
			(instruction (list mnemonic to rip)))
		   (push (assemble-instruction instruction) code)
		   (when *debug*
		     (print (list 'rip-instruction inst instruction (assemble-instruction instruction))))))
		(t (push inst code))))
	(setf (compile-component-byte-code component) (reverse code))))))

(defun link-compilation-component (compile-component start)
  (let ((current start))
    (dolist (subcomp-pair (compile-component-subcomps compile-component))
      (setf current
	    (+ current
	       (link-compilation-component (cdr subcomp-pair) current))))
    (assemble-component compile-component)
    (setf (compile-component-start compile-component) current)
    (+ current (set-and-maybe-adjust-code-size compile-component))))

(defun get-complete-component-code (component)
  (append (compile-component-prefix-code component)
	  (compile-component-byte-code component)))

(defun get-all-components-byte-code (start-component)
  (let ((code))
    (dolist (subcomp-pair (compile-component-subcomps start-component))
      (setf code
	    (get-all-components-byte-code (cdr subcomp-pair))))
    (append code (get-complete-component-code start-component))))

(defun get-all-components-rips (start-component)
  (let ((rips))
    (dolist (subcomp-pair (compile-component-subcomps start-component))
      (setf rips
	    (get-all-components-rips (cdr subcomp-pair))))
    (let ((this-rips nil)
	  (comp-rips (compile-component-rips start-component)))
      (dolist (rip comp-rips)
	(let ((rip-offset (get-rip-offset start-component (get-rip-relative-name rip))))
	  (push 
	   (make-rip-location :rip rip
			      :byte-offset (+ (compile-component-start start-component)
					      (length (first (compile-component-prefix-code start-component)))
					      (* *word-size* (- rip-offset 1))))
	   this-rips)))
      (append this-rips rips))))

(defun assemble-and-link-compilation-unit (compilation-unit start)
  (let ((start-component (compilation-unit-compile-component compilation-unit)))
    (link-compilation-component start-component start)
    (setf (compilation-unit-code compilation-unit)
	  (get-all-components-byte-code start-component))
    (setf (compilation-unit-fixups compilation-unit)
	  (get-all-components-rips start-component))
    compilation-unit))

(defun clcomp-compile (exp)
  (let* ((expanded (clcomp-macroexpand exp))
	 (nodes (create-node expanded))
	 (ir (make-ir nodes))
	 (ir-blocks (component-blocks-phase ir))
	 (assembly (make-compile-unit-and-compile-pass-1 ir-blocks))
	 (assembled-compile-unit (assemble-and-link-compilation-unit assembly 0)))
    assembled-compile-unit))


;;;; TODO
;;; - stack offsets, when we removed saving preserved regs we are wasting stack space
