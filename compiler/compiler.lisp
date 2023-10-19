(in-package #:clcomp)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (declaim (optimize (speed 0) (debug 3))))

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
;; FIXME
(defparameter *mvb-base-pointer-reg* :R11) 


(defparameter *debug* nil)

(defparameter *dont-inline* nil)

(defparameter *compile-or-load-time* nil)


(defparameter *symbols* (make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;
;;; ir to assembly
;;;
;;; ir mnemonics :
;;; LAMBDA-ENTRY
;;; ARG-CHECK
;;; ARG-MINIMUM-CHECK
;;; LISTIFY-ARGS
;;; RECEIVE-PARAM
;;; PARAMS-COUNT
;;; LOAD-PARAM
;;; LOAD-CALL
;;; VOP
;;; LOAD
;;; IF
;;; LABEL
;;; GO
;;; LAMBDA-EXIT


;;;;  STACK ;;;;
;;;;
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

(defun asm-move (loc1 loc2 allocation)
  (asm-storage-move (get-allocation-storage loc1 allocation)
		    (get-allocation-storage loc2 allocation)))

(defun calle-save-registers ()
  (let (assembly)
    (dolist (reg *preserved-regs*)
      (push (list :push reg) assembly))
    assembly))

(defun calle-restore-registers ()
  (let (assembly)
    (dolist (reg (reverse *preserved-regs*))
      (push (list :pop reg) assembly))
    assembly))

(defun maybe-allocate-stack-space (translator allocation)
  (let ((stack-size (allocation-stack allocation)))
    (if (> (allocation-stack allocation) 0)
	(emit-ir-assembly translator
			  (list
			   (list :sub *stack-pointer-reg*
				 (* *word-size* 
				    ;; reverse stack arguments fix
				    (if (oddp stack-size)
				    	stack-size
				    	(+ 1 stack-size))
				    )))) ; 16 byte aligment
	;; FIXME, remove aligment, reverse stack arguments parsing
	(emit-ir-assembly translator
			  (list (list :sub *stack-pointer-reg* *word-size*))))))

(defun maybe-deallocate-stack-space (translator allocation)
  (let ((stack-size (allocation-stack allocation)))
    (if (> (allocation-stack allocation) 0)
	(emit-ir-assembly translator
			  (list
			   (list :add *stack-pointer-reg*
				 ;; 16 byte stack alligments
				 (* *word-size*
				    ;; reverse stack arguments fix
				    (if (oddp stack-size)
				    		    stack-size
				    		    (+ 1 stack-size))
				    ))))
	;; FIXME, remove aligment, reverse stack arguments parsing
	 (emit-ir-assembly translator
       		  (list (list :add *stack-pointer-reg* *word-size*)))
	)))

(defun translate-lambda-entry (translator allocation)
  (emit-ir-assembly translator
		    (list
		     (list :push *base-pointer-reg*)
		     (list :mov *base-pointer-reg* *stack-pointer-reg*)))
;;  #+nil ; better do caller save register
  (emit-ir-assembly translator (calle-save-registers))
  (maybe-allocate-stack-space translator allocation))

(defun translate-listify-args (ir translator)
  (emit-ir-assembly translator
		    (listify-code-generator (second ir))))

(defun translate-arg-check (ir translator)
  (let ((arg-count (second ir)))
    (declare (ignore arg-count))
    (emit-ir-assembly translator
		      (list
		       (list :cmp *fun-number-of-arguments-reg* (fixnumize (second ir)))
		       (list :jump-fixup :jne :wrong-arg-count-label)))))

(defun translate-arg-minimum-check (ir translator)
  (let ((arg-count (second ir)))
    (declare (ignore arg-count))
    (emit-ir-assembly translator
		      (list
		       (list :cmp *fun-number-of-arguments-reg* (fixnumize (second ir)))
		       (list :jump-fixup :jl :wrong-arg-count-label)))))

(defun translate-params-count (ir translator)
  (let ((arguments-count (second ir)))
    (emit-ir-assembly translator
		      (list
		       (list :mov *fun-number-of-arguments-reg* (fixnumize arguments-count))))
    (when (> arguments-count (length *fun-arguments-regs*))
      (let ((stack-args (- arguments-count (length *fun-arguments-regs*))))
	(emit-ir-assembly translator
			  (list (list :sub *stack-pointer-reg*
				      ;; FIXME BACK
				      (* *word-size* 
					 (if (oddp stack-args)
					     (+ 1 stack-args)
					     stack-args)
					 ))))))))

(defun translate-receive-param (param-location param-index number-of-arguments translator allocation ir-component)
  (let ((storage (get-allocation-storage param-location allocation))
	(lambda-list-rest-p (lambda-info-lambda-list-rest (ir-component-lambda-info ir-component))))
    (emit-ir-assembly translator
		      (asm-storage-move storage
					(make-receive-param-storage param-index number-of-arguments lambda-list-rest-p)))
    (when (and lambda-list-rest-p
	       (> param-index
		  (length *fun-arguments-regs*)))
      (emit-ir-assembly translator
			(list (list :sub *fun-number-of-arguments-reg* *word-size*))))))

(defun translate-load (ir translator allocation)
  (emit-ir-assembly translator
		    (asm-move (get-load-to ir) (get-load-from ir) allocation)))

(defun translate-load-param (ir translator allocation)
  (emit-ir-assembly translator
		    (asm-move (get-load-to ir) (get-load-from ir) allocation)))

(defun translate-load-call (ir translator allocation)
  (emit-ir-assembly translator
		    (asm-storage-move (make-reg-storage :register *fun-address-reg*)
				      (make-rip-relative-storage (fourth ir))))
  (emit-ir-assembly translator
		    (list
		     (list :call *fun-address-reg*)))
  (let ((arguments-count (nth 4 ir)))
    (when (> arguments-count (length *fun-arguments-regs*))
      (let ((stack-args (- arguments-count (length *fun-arguments-regs*))))
	(emit-ir-assembly translator
			  (list (list :add *stack-pointer-reg*
				      (* *word-size* (if (oddp stack-args)
							 (+ 1 stack-args)
							 stack-args))))))))
  (let ((storage (get-allocation-storage (second ir) allocation)))
    (emit-ir-assembly translator
		      (list
		       (list :mov (storage-operand storage) *return-value-reg*)))
    #+nil(when (and storage
		    (when (typep storage 'reg-storage)
		      (not (eq (reg-storage-register storage) *return-value-reg*))))
	   (emit-ir-assembly translator
			     (list
			      (list :mov (storage-operand storage) *return-value-reg*))))))


(defun storage-to-vop-operand-type (storage)
  (etypecase storage
    (stack-storage :stack)
    (reg-storage :register)))

(defun storage-match-vop-operand (storage vop-operand)
  (let* ((vop-operand-types (rest vop-operand))	 
	 (storage-vop-operand-type (storage-to-vop-operand-type storage)))
    (find storage-vop-operand-type vop-operand-types)))

(defun get-vop-res-operand (ir-return-storage vop-res)
  (let ((storage-match (storage-match-vop-operand ir-return-storage vop-res)))
    (if storage-match
	ir-return-storage
	(make-reg-storage :register *return-value-reg*))))

(defun emit-vop-code (vop ir allocation translator)
  (let* ((arguments-storage nil)
	 (registers *fun-arguments-regs*)
	 (ir-ret-storage (get-allocation-storage (get-ir-vop-return-loc ir) allocation))
	 (ir-arguments (get-ir-vop-args ir))
	 (vop-arguments (vop-arguments vop))
 	 (vop-res (vop-res vop))	 
	 (res-operand-storage (get-vop-res-operand ir-ret-storage vop-res)))
    (dolist (argument ir-arguments)
      (let* ((arg-storage (get-allocation-storage argument allocation))
	     ;; FIXME, set vop-arguments to cdr
	     (vop-argument (first vop-arguments))
	     (vop-arg-types (rest vop-argument))
	     (storage-match (storage-match-vop-operand arg-storage vop-argument)))
	(if storage-match
	    (push (storage-operand arg-storage) arguments-storage)
	    ;; we should only have situtaion that current storage is stack and VOP needs register storageA
	    ;; FIXME, maybe first give register to vop arguments that can only be REGISTER
	    (cond ((and (stack-storage-p arg-storage)
			(= 1 (length vop-arg-types))
			(find :register vop-arg-types))
		   (let ((register (first registers)))
		     (unless register
		       (error "No more registers !!!"))
		     ;; FIXME, check this again !!!
		     (emit-ir-assembly translator (asm-storage-move (make-reg-storage :register register)
								    arg-storage))
		     (push register arguments-storage)
		     (setf registers (rest registers))))
		  (t (error "Illegal state !!!"))))))
    (let ((current-stack-ptr-op (make-stack-storage-operand (allocation-stack allocation))))
      (emit-ir-assembly translator
			(get-vop-code vop (cons (storage-operand res-operand-storage)  (reverse (cons current-stack-ptr-op
												      arguments-storage))))))
    ;; FIXME, check this
    (unless (storage-equal ir-ret-storage res-operand-storage)
      (emit-ir-assembly translator
			(asm-storage-move (get-allocation-storage (third ir) allocation)
					  res-operand-storage)))))

(defun translate-vop-call (ir translator allocation)
  (let ((vop-name (get-ir-vop-name ir))
	(ir-vop-args (get-ir-vop-args ir)))
    (if vop-name
	(let ((vop (get-vop vop-name)))
	  (if vop
	      (let ((vop-arguments-size (length (vop-arguments vop))))
		(when (not (= (length ir-vop-args) vop-arguments-size))
		  (error (format nil "Wrong number of arguments for VOP in ~a" ir)))
		(emit-vop-code vop ir allocation translator))
	      (error (format nil "Unknown vop for ir ~a" ir))))
	(error (format nil "Unknown vop for isrr ~a" ir)))))

(defun translate-if (ir translator allocation)
  (let* ((location (second ir))
	 (storage (get-allocation-storage location allocation))
	 (storage-type (type-of storage))
	 (test-storage storage))
    (when (or
	   (eq storage-type 'stack-storage)
	   (eq storage-type 'memory-storage))
      (emit-ir-assembly translator
			(asm-storage-move (make-reg-storage :register *tmp-reg*) storage ))
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
  (emit-ir-assembly translator (list (list :ud2))))

(defun to-asm (ir translator allocation ir-component)
  (let ((mnemonic (first ir)))
    (when *debug*
      (progn
	(format t "~%~%---------------------------------------------------")
	(format t "~%~%")
	(print ir)
	(format t "~%")))
    (case mnemonic
      (lambda-entry (translate-lambda-entry translator allocation))
      (listify-args (translate-listify-args ir translator))
      (arg-check (translate-arg-check ir translator))
      (arg-minimum-check (translate-arg-minimum-check ir translator))
      (params-count (translate-params-count ir translator))
      (receive-param (translate-receive-param (second ir) (third ir) (fourth ir) translator allocation ir-component))
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
	(let ((ir-instruction (cdr ir))
	      (index (car ir)))
	  (when *debug*
	    (format t "~% ********* INDEX: ~a~%" index))
	  (to-asm ir-instruction translator allocation ir-component))))
    translator))

;;; linear scan register allocation
;;; LIFETIME ANALYSIS

(defun register-locations-used (block locations)
  (dolist (loc locations)
    (let ((loc-sym (location-symbol loc)))
      (setf (ir-block-use block)
	    (adjoin loc-sym (ir-block-use block))))))

(defun maybe-kill-used (block defined)
  (let ((defined (mapcar #'location-symbol defined)))
    (setf (ir-block-def block)
	  (union defined (ir-block-def block)))
    (setf (ir-block-use block)
	  (set-difference (ir-block-use block) defined))))

(defun finish-block (block)
  (setf (ir-block-in block)
	(union (ir-block-use block)
	       (set-difference (ir-block-out block)
			       (ir-block-def block)))))

(defun analyse-block-lifetime (block)
  (dolist (ir-cons (reverse (ir-block-ir block)))
    (let ((locations (get-ir-locations (cdr ir-cons))))
      (when locations
	(destructuring-bind (defined used) locations
	  (when defined
	    (maybe-kill-used block defined))
	  (when used
	    (register-locations-used block used))))))
  (finish-block block))

(defun lifetime-trace-block (block indexed-blocks)
  (let ((from-blocks (ir-block-from-blocks block)))
    (dolist (fb-index from-blocks)
      (let ((fb (gethash fb-index indexed-blocks)))
	(if (null fb)
	    (error "fb is null !!")
	    (let* ((fb-out (ir-block-out fb))
		   (new-out (union fb-out (ir-block-in block))))
	      (when (set-difference new-out fb-out)
		(setf (ir-block-out fb) new-out)
		(finish-block fb)
		(lifetime-trace-block fb indexed-blocks))))))))

(defun lifetime-trace-analysis (ir-component indexed-blocks)
  (let ((blocks (reverse (ir-component-code-blocks ir-component))))
    (dolist (block blocks)
      (lifetime-trace-block block indexed-blocks))))

(defun do-lifetime-analysis (ir-component)
  (let* ((blocks (ir-component-code-blocks ir-component))
	 (indexed-blocks (make-block-by-index-map blocks)))
    (dolist (block blocks)
      (analyse-block-lifetime block))
    (lifetime-trace-analysis ir-component indexed-blocks)))

;;; CREATE LIFETIME INTERVALS

(defstruct interval sym intervals)

(defun get-interval-first-start (interval)
  (first (first (interval-intervals interval))))

(defun get-interval-first-end (interval)
  (second (first (interval-intervals interval))))

(defun collect-intervals-by-name (intervals)
  (let ((map (make-hash-table)))
    (dolist (int intervals)
      (let ((name (first int))
	    (start (second int))
	    (end (third int)))
	(let ((existing (gethash name map)))
	  (if existing
	      (setf (gethash name map) (sort
					(cons (list start end) existing)
					#'<
					:key #'first))
	      (setf (gethash name map) (list (list start end)))))))
    map))

(defun merge-interval-list (interval-list)
  (let ((sorted-intervals (sort interval-list #'< :key #'first))
	(result nil)
	(last-int nil))
    (dolist (int sorted-intervals)
      (if last-int
	  (if (= (+ 1 (second last-int)) (first int))
	      (setf (second last-int) (second int))
	      (progn
		(push last-int result)
		(setf last-int int)))
	  (setf last-int int)))
    (push last-int result)
    (reverse result)))

(defun merge-add-interval (var map start end)
  (let ((existing (gethash var map)))
    (if existing
	(setf (gethash var map)
	      (cons (list start end) (gethash var map)))
	(setf (gethash var map) (list (list start end))))))

(defun merge-intervals (intervals)
  (let ((imap (make-hash-table)))
    (dolist (int intervals)
      (merge-add-interval (first int) imap (second int) (third int)))
    (let ((hkeys (hash-keys imap)))
      (dolist (k hkeys)
	(setf (gethash k imap)
	      (merge-interval-list (gethash k imap)))))
    imap))

(defun get-block-intervals (ir-component)
  (let ((complete-intervals nil))
    (dolist (block (ir-component-code-blocks ir-component))
      (let ((in (ir-block-in block))
	    (out (ir-block-out block))
	    (first-index (first (first (ir-block-ir block))))
	    (last-index (first (car (last (ir-block-ir block)))))
	    (uncomplete-intervals (make-hash-table)))
	(dolist (v in)
	  (let ((v-interval (gethash v uncomplete-intervals)))
	    (unless v-interval
	      (setf (gethash v uncomplete-intervals) (cons first-index nil)))))
	(dolist (ir-list (ir-block-ir block))
	  (let ((index (first ir-list))
		(ir (rest ir-list)))
	    (let ((locs (get-ir-locations ir)))
	      (when locs
		(destructuring-bind (defined used) locs
		  (when defined
		    (dolist (def (mapcar #'location-symbol defined))
		      (let ((ui (gethash def uncomplete-intervals)))
			(if ui
			    (setf (cdr ui) index)
			    (setf (gethash def uncomplete-intervals) (cons index nil))))))
		  (when used
		    (dolist (use (mapcar #'location-symbol used))
		      (unless (find use out)
			(let ((vint (gethash use uncomplete-intervals)))
			  (if (not vint)
			      (error (concatenate 'string "We don't have interval for " (symbol-name use)))
			      (setf (cdr vint) index)))))))))))
	(let ((uncomplete-keys (hash-keys uncomplete-intervals)))
	  (dolist (uk uncomplete-keys)
	    (let ((inter (gethash uk uncomplete-intervals)))
	      (let ((start (car inter))
		    (end (cdr inter)))
		(if (not (find uk out))
		    (push (list uk start (or end start)) complete-intervals)
		    (push (list uk start last-index) complete-intervals))))))))
    complete-intervals))

(defun make-ir-intervals (ir-component)
  (do-lifetime-analysis ir-component)
  (let ((block-intervals (get-block-intervals ir-component)))
    (merge-intervals block-intervals)))

;;; ALOCATION
(defstruct reg-storage register)
;;; FIXME, no need for stack-storage, should be memory-storage with RBP as base
(defstruct stack-storage offset)
(defstruct memory-storage base index scale offset)
(defstruct constant-storage constant)

(defun allocation-add-storage (allocation interval storage)
  (setf (gethash (interval-sym interval) (allocation-storage allocation))
	storage))

(defun get-interval-register (allocation interval)
  (gethash (interval-sym interval) (allocation-reg-map allocation)))

(defun put-interval-register (allocation interval register)
  (setf (gethash (interval-sym interval) (allocation-reg-map allocation))
	register))

(defun get-sorted-intervals-list (intervals-map)
  (let (res)
    (maphash (lambda (k v)
	       (push (make-interval :sym k :intervals v) res))
	     intervals-map)
    (sort res #'< :key (lambda (i) (first (first (interval-intervals i)))))))

(defun interval-is-handled (interval index)
  (every (lambda (i)
	   (< (second i) index))
	 (interval-intervals interval)))

(defun interval-is-active (interval index)
  (some (lambda (i)
	  (and (<= (first i) index)
	       (>= (second i) index)))
	(interval-intervals interval)))

(defun interval-is-inactive (interval index)
  (and (not (interval-is-handled interval index))
       (not (interval-is-active interval index))))

(defun interval-dont-intersect (i1 i2)
  (let ((i1-start (first i1))
	(i1-end (second i1))
	(i2-start (first i2))
	(i2-end (second i2)))
    (or (< i1-end i2-start)
	(> i1-start i2-end))))

(defun intervals-dont-intersect (i1 i2)
  (dolist (int1 (interval-intervals i1))
    (dolist (int2 (interval-intervals i2))
      (when (not (interval-dont-intersect int1 int2))
	(return-from intervals-dont-intersect nil))))
  t)

(defun sort-allocation-active (allocation)
  (setf (allocation-active allocation)
	(sort (allocation-active allocation)
	      #'< :key (lambda (i)
			 (second (first (interval-intervals i))))))
  allocation)

;;; FIXME check last of active intervals like in Linear Scan Allocation paper !!!
(defun spill-something (allocation interval)
  (allocation-add-storage allocation interval
			  (make-stack-storage :offset (allocation-stack allocation)))
  (incf (allocation-stack allocation)))

(defstruct allocation registers stack active inactive handled reg-map storage shared-reg-counter)

(defun register-shared-register (allocation register)
  (if (gethash register (allocation-shared-reg-counter allocation))
      (incf (gethash register (allocation-shared-reg-counter allocation)))
      (setf (gethash register (allocation-shared-reg-counter allocation)) 0)))

(defun deregister-shared-register (allocation register)
  (let ((cnt (gethash register (allocation-shared-reg-counter allocation))))
    (if (zerop cnt)
	(remhash register (allocation-shared-reg-counter allocation))
	(decf (allocation-shared-reg-counter allocation)))))

(defun maybe-return-register (allocation interval)
  (let ((register (get-interval-register allocation interval)))
    (unless (gethash register (allocation-shared-reg-counter allocation))
      (push register (allocation-registers allocation)))))

(defun maybe-activate-inactive (allocation index)
  (let ((new-inactive nil))
    (dolist (inactive-interval (allocation-inactive allocation))
      (cond ((interval-is-active inactive-interval index)
	     (push inactive-interval (allocation-active allocation))
	     (deregister-shared-register allocation (get-interval-register allocation inactive-interval)))
	    ((interval-is-handled inactive-interval index)
	     (push inactive-interval (allocation-handled allocation))
	     (deregister-shared-register allocation (get-interval-register allocation inactive-interval))
	     (maybe-return-register allocation inactive-interval))
	    (t (push inactive-interval new-inactive))))
    (setf (allocation-inactive allocation) new-inactive)
    (sort-allocation-active allocation)))

(defun search-for-inactive-register (allocation interval)
  (let ((maybe-regs (hash-keys (allocation-shared-reg-counter allocation)))
	(reg-cnt-map (make-hash-table)))
    (dolist (reg (hash-keys (allocation-shared-reg-counter allocation)))
      (setf (gethash reg reg-cnt-map)
	    (gethash reg (allocation-shared-reg-counter allocation))))
    (if (null maybe-regs)
	nil
	(dolist (inactive-interval (allocation-inactive allocation))
	  (when (null maybe-regs)
	    (return-from search-for-inactive-register nil))
	  (let ((reg (get-interval-register allocation inactive-interval)))
	    (when reg
	      (if (intervals-dont-intersect interval inactive-interval)
		  (progn
		    (decf (gethash reg reg-cnt-map))
		    (when (zerop (gethash reg reg-cnt-map))
		      (return-from search-for-inactive-register reg)))
		  (setf maybe-regs (set-difference maybe-regs (list reg))))))))))


(defun find-register (allocation interval)
  ;; (when (equalp (interval-intervals interval) (list (list 11 11))) (break))
  (let ((inactive-register (search-for-inactive-register allocation interval)))
    (if inactive-register
	(progn
	  (register-shared-register allocation inactive-register)
	  inactive-register)
	(let ((registers (allocation-registers allocation)))
	  (when registers
	    (setf (allocation-registers allocation)
		  (cdr registers))
	    (car registers))))))

(defun maybe-allocate-register-storage (allocation interval)
  ;; (when (equalp (interval-intervals interval) (list (list 12 13))) (break))
  (let ((register (find-register allocation interval)))
    (when register
      (push interval (allocation-active allocation))
      (sort-allocation-active allocation)
      (put-interval-register allocation interval register)
      (allocation-add-storage allocation interval
			      (make-reg-storage :register register)))))


(defun maybe-expire-intervals (allocation index)
  (dolist (interval (allocation-active allocation))
    ;; active list is sorted
    (if (not (interval-is-active interval index))
	(progn
	  (setf (allocation-active allocation)
		(cdr (allocation-active allocation)))
	  (if (interval-is-handled interval index)
	      (progn
		(push interval (allocation-handled allocation))
		(maybe-return-register allocation interval))
	      (progn
		(push interval (allocation-inactive allocation))
		(register-shared-register allocation (get-interval-register allocation interval)))))
	(return-from maybe-expire-intervals allocation))))

(defun make-component-allocation (intervals-map)
  (let ((allocation (make-allocation  :stack 0
				      :reg-map (make-hash-table)
				      :registers *preserved-regs*
				      :storage (make-hash-table)
				      :shared-reg-counter (make-hash-table))))
    (dolist (interval (get-sorted-intervals-list intervals-map))
      (let ((interval-start (get-interval-first-start interval)))
	(maybe-activate-inactive allocation interval-start)
	(maybe-expire-intervals allocation interval-start)
	(unless (maybe-allocate-register-storage allocation interval)
	  (spill-something allocation interval))))
    allocation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-stack-storage-operand (offset)
  (@ *base-pointer-reg* nil nil (- (* *word-size* (+ offset
						     (length *preserved-regs*)
						     1)))))

(defun bump-stack-operand (stack-operand number-of-words)
  (@ (first stack-operand)
     (second stack-operand)
     (third stack-operand)
     (- (fourth stack-operand) (* number-of-words *word-size*))))

(defun storage-operand (storage)
  (case (type-of storage)
    (constant-storage (constant-storage-constant storage))
    (reg-storage (reg-storage-register storage))
    (stack-storage (make-stack-storage-operand (stack-storage-offset storage)))
    (memory-storage (@ (memory-storage-base storage)
		       (memory-storage-index storage)
		       (memory-storage-scale storage)
		       (memory-storage-offset storage)))))

(defun make-receive-param-storage (param-index number-of-arguments lambda-list-rest-p)
  (let ((regs-args (length *fun-arguments-regs*)))
    (if (> param-index regs-args)
	;; we already did push RBP to stack and with return addres that's 2 slots so arguments are RBP+16, RBP+24 ...
	(if  lambda-list-rest-p
	     (make-memory-storage :base *base-pointer-reg*
				  :index *fun-number-of-arguments-reg*)
	     (make-memory-storage :base *base-pointer-reg*
				  :offset (+ (* 2 *word-size*)
					     (* *word-size* (- number-of-arguments param-index)))))
	(make-reg-storage :register (nth (- param-index 1) *fun-arguments-regs*)))))

(defun get-param-storage (param-number arguments-count)
  (let ((arg-reg-count (length *fun-arguments-regs*)))
    (if (> param-number arg-reg-count)
	(make-memory-storage :base *stack-pointer-reg* :offset (* *word-size*
								  (- arguments-count param-number)))
	(make-reg-storage :register (nth (- param-number 1) *fun-arguments-regs*)))))

(defun make-rip-relative-storage (location)
  (make-memory-storage :base :RIP
		       :offset (list 'displacement (list 'rip location))))

(defun get-allocation-storage (location allocation)
  (let ((location-type (type-of location)))
    (case location-type
      (immediate-constant (make-constant-storage :constant (immediate-constant-constant location)))
      (param-location (get-param-storage (param-location-param-number location)
					 (param-location-arguments-count location)))
      (ret-location (gethash (location-symbol location) (allocation-storage allocation))
       #+nil(or
	     (gethash (location-symbol location) (allocation-storage allocation))
	     (make-reg-storage :register *return-value-reg*)
	     )
       )
      ((var-location tmp-location) (gethash (location-symbol location) (allocation-storage allocation)))
      (rip-relative-location (make-rip-relative-storage (rip-relative-location-location location)))
      (lambda-return-location (make-reg-storage :register *return-value-reg*))
      (otherwise (error "Unknown location type")))))

(defun storage-equal (s1 s2)
  (and (eq (type-of s1)
	   (type-of s2))
       (etypecase s1
	 (reg-storage (eq (reg-storage-register s1)
			  (reg-storage-register s2)))
	 (stack-storage (equal (storage-operand s1)
			       (storage-operand s2))))))



(defstruct rip-location rip byte-offset)
(defstruct compile-component id code prefix-code start code-size subcomps rips rip-offsets byte-code)
(defstruct compilation-unit compile-component start fixups code main-offset eval-at-load)

(defun get-compilation-unit-code-size (compilation-unit)
  (reduce (lambda (s c)
	    (+ s (length c)))
	  (compilation-unit-code compilation-unit)
	  :initial-value 0))

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
  (let* ((intervals (make-ir-intervals ir-component))
	 (allocation (make-component-allocation intervals)))
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

(defun index-ir-component-instr (ir-component)
  (let ((index 1))
    (dolist (block (ir-component-code-blocks ir-component))
      (let ((indexed-ir nil))
	(dolist (ir (ir-block-ir block))
	  (push (cons index ir) indexed-ir)
	  (incf index))
	(setf (ir-block-ir block) (reverse indexed-ir))))))

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
    (index-ir-component-instr ir-component)
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
						(or (typep rip 'component-rip-relative)
						    (typep rip 'fixup-rip-relative)))
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
	    (when (not (or (typep rip-relative 'component-rip-relative)
			   (typep rip-relative 'fixup-rip-relative)))
	      (push *dummy-rip-value* pre-code)
	      (push (list (get-rip-relative-name rip-relative) start-offset) rip-offsets)
	      (decf start-offset)))))
      (setf (compile-component-byte-code component)
	    main-code)
      (setf (compile-component-prefix-code component)
	    (reverse pre-code))
      (setf (compile-component-rip-offsets component)
	    rip-offsets)
      (resolve-component-rips component))))

(defun get-rip-offset (component rip-name)
  (or (second (assoc rip-name (compile-component-rip-offsets component)))
      (error (concatenate 'string "Error, unknown RIP " rip-name))))

(defun calculate-rip-offset (current-code-size rip-name component)
  ;; add current code size + all rip offsets - (actual rip offset - 1)
  ;; most distant rip is on offset 1
  (let ((rip-offset (get-rip-offset component rip-name))
	(rips-size (length (compile-component-rip-offsets component))))
    (- (- current-code-size
	  (- (* *word-size* (- rips-size (- rip-offset 1))))))))

;;; Actually assemble loading data from RIP relative address
(defun resolve-component-rips (component)
  (let ((rip-offsets (compile-component-rip-offsets component)))
    (when rip-offsets
      (let ((code nil)
	    (current-size 0))
	(dolist (inst (compile-component-byte-code component))
	  ;; FIXME, check this again
	  (incf current-size (instruction-size inst))
	  (cond ((instruction-has-unresolved-memory-operand inst)
		 (let* ((rip-name (unresolved-memory-operand-rip-name inst))
			(rip-offset (calculate-rip-offset current-size
							  rip-name
							  component))
			(resolved-instruction (resolve-instruciton-offset inst rip-offset)))
		   (push (assemble-instruction resolved-instruction) code)))
		(t (push inst code))))
	(setf (compile-component-byte-code component) (reverse code))))))

(defun flatten-comps (components &optional (key #'cdr))
  (if (null components)
      nil
      (append
       (cons (funcall key (car components))
	     (flatten-comps (compile-component-subcomps (cdr (car components))) key))
       (flatten-comps (cdr components) key))))

(defun flatten-components-in-order (start-compile-component)
  (reverse (cons start-compile-component
		 (flatten-comps (compile-component-subcomps start-compile-component)))))

(defun link-compilation-component (compile-component start)
  (let ((flatten-components (flatten-components-in-order compile-component))
	(current start))
    (dolist (component flatten-components)
      (assemble-component component)
      (setf (compile-component-start component) current)
      (setf current (+ current (set-and-maybe-adjust-code-size component))))))

(defun get-complete-component-code (component)
  (append (compile-component-prefix-code component)
	  (compile-component-byte-code component)))

(defun get-all-components-byte-code (start-component)
  (let ((flatten-components (flatten-components-in-order start-component))
	(code nil))
    (dolist (component flatten-components)
      (setf code (append code (get-complete-component-code component))))
    code))

(defun get-all-components-rips (start-component)
  (let ((subcomps (cons start-component (flatten-comps (compile-component-subcomps start-component)
						       #'cdr)))
	(rips nil))
    (dolist (comp subcomps)
      (let* ((comp-rips (compile-component-rips comp)))
	(dolist (crip comp-rips)
	  (let ((rip-offset (get-rip-offset comp (get-rip-relative-name crip))))
	    (push 
	     (make-rip-location :rip crip
				:byte-offset (+ (compile-component-start comp)
						(length (first (compile-component-prefix-code comp)))
						(* *word-size* (- rip-offset 1))))
	     rips)))))
    rips))

(defun assemble-and-link-compilation-unit (compilation-unit start)
  (let ((start-component (compilation-unit-compile-component compilation-unit)))
    (link-compilation-component start-component start)
    (setf (compilation-unit-code compilation-unit)
	  (get-all-components-byte-code start-component))
    (setf (compilation-unit-fixups compilation-unit)
	  (get-all-components-rips start-component))
    compilation-unit))

(defun %compiler-defun (f)
  (declare (ignore f)))

(defun clcomp-compile (name exp &key (eval-at-load nil))
  (let* ((expanded (clcomp-macroexpand exp))
	 (nodes (create-node expanded))
	 (ir (make-ir nodes))
	 (ir-blocks (component-blocks-phase ir))
	 (assembly (make-compile-unit-and-compile-pass-1 ir-blocks))
	 (assembled-compile-unit (assemble-and-link-compilation-unit assembly 0)))
    (when (and eval-at-load
	       (not name))
      (setf (compilation-unit-eval-at-load assembled-compile-unit) t))
    (rt-add-to-compilation assembled-compile-unit)
    (maybe-rt-%defun name assembled-compile-unit)
    assembled-compile-unit))


(defun ssa-clcomp-compile (name exp &key (eval-at-load nil))
  (let* ((lambda-ssa (clcomp.ssa::lambda-construct-ssa (create-node (clcomp-macroexpand exp))))
	 (intervals (clcomp.ssa::build-intervals lambda-ssa))
	 (alloc (clcomp.ssa::linear-scan intervals)))
    (clcomp.ssa::resolve-data-flow lambda-ssa alloc)
    (clcomp.ssa::translate-to-asm lambda-ssa alloc)
    (let* ((compile-unit (clcomp.translator::translate-to-compilation-unit lambda-ssa))
	   (assembled-compile-unit (assemble-and-link-compilation-unit compile-unit 0)))
      (when (and eval-at-load
		 (not name))
	(setf (compilation-unit-eval-at-load assembled-compile-unit) t))
      (rt-add-to-compilation assembled-compile-unit)
      (maybe-rt-%defun name assembled-compile-unit)
      assembled-compile-unit)))

;;;; TODO
;;; - stack offsets, when we removed saving preserved regs we are wasting stack space

(defun test-reg-allocation (reg locs intervals)
  (declare (ignore reg))
  (dolist (loc locs)
    (dolist (loc2 locs)
      (unless (eq loc loc2)
	(let ((i1 (make-interval :sym loc :intervals (gethash loc intervals)))
	      (i2 (make-interval :sym loc2 :intervals (gethash loc2 intervals))))
	  (unless (intervals-dont-intersect i1 i2)
	    (print (list i1 i2))))))))

(defun test-allocation (allocation intervals)
  (let ((th (make-hash-table))
	(locs (hash-keys (allocation-storage allocation))))
    (dolist (l locs)
      (let ((storage (gethash l (allocation-storage allocation))))
	(when (typep storage 'reg-storage)
	  (setf (gethash (reg-storage-register storage) th)
		(cons l (gethash (reg-storage-register storage) th))))))
    (let ((regs (hash-keys th)))
      (dolist (reg regs)
	(test-reg-allocation reg (gethash reg th) intervals)))))
