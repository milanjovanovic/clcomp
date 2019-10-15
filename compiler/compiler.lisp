(in-package #:clcomp)

(declaim (optimize (speed 0) (debug 3)))

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

(defparameter *c* nil)
(defparameter *all* nil)
(defparameter *ir* nil)

;;;;;;;;;;;;;;;;;;;;;;
;;; ir to assembly
;;;
;;; ir mnemonics :
;;; LAMBDA-ENTRY
;;; ARG-CHECK
;;; LISTIFY-ARGS FIXME, not implemented yet
;;; RECEIVE-PARAM
;;; PARAMS-COUNT
;;; LOAD-PARAM
;;; LOAD-CALL
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
   (when (> (allocation-stack allocation) 0)
     (emit-ir-assembly translator
		       (list
			(list :sub *stack-pointer-reg*
			      (* *word-size* (if (oddp stack-size)
						 (+ 1 stack-size)
						 stack-size))))))))

(defun maybe-deallocate-stack-space (translator allocation)
  (let ((stack-size (allocation-stack allocation)))
   (when (> (allocation-stack allocation) 0)
     (emit-ir-assembly translator
		       (list
			(list :add *stack-pointer-reg*
			      ;; 16 byte stack alligments
			      (* *word-size* (if (oddp stack-size)
						 (+ 1 stack-size)
						 stack-size))))))))

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
		     (list :call *fun-address-reg*)))
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
	(let ((ir-instruction (cdr ir))
	      (index (car ir)))
	  (when *debug*
	    (format t "~% ********* INDEX: ~a~%" index))
	  (to-asm ir-instruction translator allocation))))
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
  (setf *c* ir-component)
  (do-lifetime-analysis ir-component)
  (let ((block-intervals (get-block-intervals ir-component)))
    (merge-intervals block-intervals)))

;;; ALOCATION

(defstruct allocation active inactive handled reg-map registers inactive-registers storage stack)
(defstruct reg-storage register)
;;; FIXME, no need for stack-storage, should be memory-storage with RBP as base
(defstruct stack-storage offset)
(defstruct memory-storage base offset)
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

(defun interval-remove-first (interval)
  (make-interval :sym (interval-sym interval)
		 :intervals (cdr (interval-intervals interval))))

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

(defun maybe-activate-inactive (allocation index)
  (do ((inactive (allocation-inactive allocation) (cdr inactive)))
      ((or (null inactive)
	   (not (interval-is-active (car inactive) index)))
       (setf (allocation-inactive allocation) inactive))
    (push (car inactive) (allocation-active allocation))
    (delete-inactive-register allocation (car inactive)))
  (sort-allocation-active allocation)
  allocation)

(defun sort-allocation-inactive (allocation)
  (setf (allocation-inactive allocation)
	(sort (allocation-inactive allocation)
	      #'< :key (lambda (i)
			 (first (first (interval-intervals i))))))
  allocation)

(defun sort-allocation-active (allocation)
  (setf (allocation-active allocation)
	(sort (allocation-active allocation)
	      #'< :key (lambda (i)
			 (second (first (interval-intervals i))))))
  allocation)

(defun return-inactive-register (allocation interval)
  (setf (gethash (get-interval-register allocation interval)
		 (allocation-inactive-registers allocation))
	'free))

(defun delete-inactive-register (allocation interval)
  (remhash (get-interval-register allocation interval)
	   (allocation-inactive-registers allocation)))

(defun take-inactive-register (allocation interval)
  (setf (gethash (get-interval-register allocation interval)
		 (allocation-inactive-registers allocation))
	'taken))

(defun inactive-register-free (allocation interval)
  (eq (gethash (get-interval-register allocation interval)
	       (allocation-inactive-registers allocation))
      'free))

(defun maybe-return-inactive-register (allocation interval)
  (when (gethash (get-interval-register allocation interval)
		 (allocation-inactive-registers allocation))
    (return-inactive-register allocation interval)))

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
		(or (maybe-return-inactive-register allocation interval)
		    (push (get-interval-register allocation interval)
			  (allocation-registers allocation))))
	      (progn
		(push (interval-remove-first interval) (allocation-inactive allocation))
		(return-inactive-register allocation interval)
		(sort-allocation-inactive allocation))))
	(return-from maybe-expire-intervals allocation))))


(defun find-inactive-interval (allocation interval)
  (declare (optimize (speed 0) (debug 3)))
  ;; (when (equalp (interval-intervals interval) (list (list 11 11))) (break))
  (dolist (inactive (allocation-inactive allocation))
    (when (and
	   (intervals-dont-intersect inactive interval)
	   (inactive-register-free allocation inactive))
      (take-inactive-register allocation inactive)
      (return-from find-inactive-interval inactive))))

(defun find-register (allocation interval)
  ;; first try inactive intervals ???
  (declare (optimize (speed 0) (debug 3)))
  ;; (when (equalp (interval-intervals interval) (list (list 11 11))) (break))
  (let ((inactive-interval (find-inactive-interval allocation interval)))
    (if inactive-interval
	(get-interval-register allocation inactive-interval)
	(let ((registers (allocation-registers allocation)))
	  (when registers
	    (setf (allocation-registers allocation)
		  (cdr registers))
	    (car registers))))))

;;; FIXME check last of active intervals like in Linear Scan Allocation paper !!!
(defun spill-something (allocation interval)
  (allocation-add-storage allocation interval
			  (make-stack-storage :offset (allocation-stack allocation)))
  (incf (allocation-stack allocation))
  (push interval (allocation-active allocation))
  (sort-allocation-active allocation))

(defun maybe-allocate-register-storage (allocation interval)
  (declare (optimize (speed 0) (debug 3)))
  ;; (when (equalp (interval-intervals interval) (list (list 12 13))) (break))
  (let ((register (find-register allocation interval)))
    (when register
      (push interval (allocation-active allocation))
      (sort-allocation-active allocation)
      (put-interval-register allocation interval register)
      (allocation-add-storage allocation interval
			      (make-reg-storage :register register)))))

(defun make-component-allocation (intervals-map)
  (declare (optimize (speed 0) (debug 3)))
  (let ((allocation (make-allocation  :stack 0
				      :reg-map (make-hash-table)
				      :registers *preserved-regs*
				      :inactive-registers (make-hash-table)
				      :storage (make-hash-table))))
    (dolist (interval (get-sorted-intervals-list intervals-map))
      (maybe-activate-inactive allocation (get-interval-first-start interval) )
      (maybe-expire-intervals allocation (get-interval-first-start interval))
      (unless (maybe-allocate-register-storage allocation interval)
	(spill-something allocation interval)))
    allocation))

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
      ;; we already did push RBP to stack and with return addres that's 2 slots so arguments are RBP+16, RBP+24 ...
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
      (ret-location (or #+nil(gethash (location-symbol location) (allocation-storage allocation))
			(make-reg-storage :register *return-value-reg*)))
      ((var-location tmp-location) (gethash (location-symbol location) (allocation-storage allocation)))
      (rip-relative-location (make-memory-storage))
      (lambda-return-location (make-reg-storage :register *return-value-reg*))
      (otherwise (error "Unknown location type")))))



(defstruct rip-location rip byte-offset)
(defstruct compile-component code prefix-code start code-size subcomps rips rip-offsets byte-code) 
(defstruct compilation-unit compile-component start fixups code)

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
    (setf *c* ir-component
	  *ir* intervals
	  *all* allocation)
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
	  (cond ((eq (first inst) :rip-relative-fixup)
		 (let* ((mnemonic (second inst))
			(to (third inst))
			(rip-name (second (fourth inst)))
			(rip (list *instruction-pointer-register* nil nil (calculate-rip-offset current-size
												rip-name
												component)))
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

(defun %compiler-defun (f)
  (declare (ignore f)))

(defun clcomp-compile (name exp)
  (let* ((expanded (clcomp-macroexpand exp))
	 (nodes (create-node expanded))
	 (ir (make-ir nodes))
	 (ir-blocks (component-blocks-phase ir))
	 (assembly (make-compile-unit-and-compile-pass-1 ir-blocks))
	 (assembled-compile-unit (assemble-and-link-compilation-unit assembly 0)))
    (rt-add-to-compilation assembled-compile-unit)
    (when name
      (rt-%defun name assembled-compile-unit))
    assembled-compile-unit))


;;;; TODO
;;; - stack offsets, when we removed saving preserved regs we are wasting stack space
