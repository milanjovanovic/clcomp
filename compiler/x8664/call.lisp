(in-package :clcomp)

(define-vop %set-rip-value (res :register) ((rip-location :register) (lambda :register))
  ;; FIXME
  )

(defun generate-function-prologue ()
  (let ((*segment-instructions* nil))
    (inst :push *base-pointer-reg*)
    (inst :mov *base-pointer-reg* *stack-pointer-reg*)
    (dolist (reg (reverse *preserved-regs*))
      (inst :push reg))
    (reverse *segment-instructions*)))

(defun generate-function-epilogue ()
  (let ((*segment-instructions* nil))
    (dolist (reg *preserved-regs*)
      (inst :pop reg))
    (inst :pop *base-pointer-reg*)
    (reverse *segment-instructions*)))

;;; FIXME, APPLY will not work with MULTIPLE VALUES
(define-vop %apply (res :register :stack)
  ((fun :register :stack)
   (arguments :register :stack))
  (let* ((loopl (make-vop-label "loop-"))
	 (startl (make-vop-label "startl-"))
	 (end-label (make-vop-label "end-label-"))
	 (rdi (make-vop-label "rdi-"))
	 (r8 (make-vop-label "r8-"))
	 (r9 (make-vop-label "r9-"))
	 (stack-args-label (make-vop-label "stack-args-label-"))
	 (no-stack-args-label (make-vop-label "no-stack-args-label"))
	 (exit-label (make-vop-label "exit-label"))
	 (fun-stack-ptr (bump-stack-operand $stack-top-operand$ 1))
	 (fun-is-reg (find fun *fun-arguments-regs*))
	 (new-stack-top (if fun-is-reg
			    (bump-stack-operand fun-stack-ptr 1)
			    fun-stack-ptr)))

    (inst :mov *tmp-reg* arguments)
    (inst :mov *fun-number-of-arguments-reg* 0)

    ;; make space to save RCX
    ;; save two words if we need to save fun on stack
    (inst :sub *stack-pointer-reg*
	  (if fun-is-reg
	      (* 2 *word-size*)
	      *word-size*))

    (when fun-is-reg
      (inst :mov fun-stack-ptr fun))

    (inst :jump-fixup :jmp startl)

    (inst :label loopl)
    (inst :inc *fun-number-of-arguments-reg*)
    (inline-vop 'cdr *tmp-reg* *tmp-reg* $stack-top-operand$)

    (inst :label startl)

    (inst :cmp *tmp-reg* *nil*)
    (inst :jump-fixup :je end-label)
      
    (inst :cmp *fun-number-of-arguments-reg* 4)
    (inst :jump-fixup :jge stack-args-label)
      
    (inst :cmp *fun-number-of-arguments-reg* 3)
    (inst :jump-fixup :je r9)
      
    (inst :cmp *fun-number-of-arguments-reg* 2)
    (inst :jump-fixup :je r8)
      
    (inst :cmp *fun-number-of-arguments-reg* 1)
    (inst :jump-fixup :je rdi)

    (inline-vop 'car :rdx *tmp-reg* new-stack-top)
    (inst :jump-fixup :jmp loopl)

    (inst :label rdi)
    (inline-vop 'car :rdi *tmp-reg* new-stack-top)
    (inst :jump-fixup :jmp loopl)

    (inst :label r8) 
    (inline-vop 'car :r8 *tmp-reg* new-stack-top)
    (inst :jump-fixup :jmp loopl)

    (inst :label r9)
    (inline-vop 'car :r9 *tmp-reg* new-stack-top)
    (inst :jump-fixup :jmp loopl)

    (inst :label stack-args-label)
    (inline-vop 'car :r11 *tmp-reg* new-stack-top)
    (inst :push :r11) 
    (inst :jump-fixup :jmp loopl)

    (inst :label end-label)
    
    ;; save RCX
    (inst :mov $stack-top-operand$ *fun-number-of-arguments-reg*)

    ;; no longer using fixnum in RCX
    ;; (inst :shl *fun-number-of-arguments-reg* *tag-size*)
    
    (inst :mov *fun-address-reg* (if fun-is-reg
				     fun-stack-ptr
				     fun))
    
    ;; FIXME, it's symbol address
    (inst :call *fun-address-reg*)

    ;; calculate  how much stack to give back
    (inst :mov *tmp-reg* $stack-top-operand$)
    (inst :sub *tmp-reg* (length *fun-arguments-regs*))
    (inst :jump-fixup :js no-stack-args-label)
    
    (inst :lea *tmp-reg* (@ nil *tmp-reg* *word-size* (* *word-size*
							 (if fun-is-reg 2 1))))
    (inst :jump-fixup :jmp exit-label)

    (inst :label no-stack-args-label)
    (inst :mov *tmp-reg* (* *word-size*
			    (if fun-is-reg 2 1)))

    (inst :label exit-label)
    
    (inst :add *stack-pointer-reg* *tmp-reg*)
    (inst :mov res *return-value-reg*))
    
  (reverse *segment-instructions*))

(defun listify-code-generator (fixed-arguments-count)
  (let ((*segment-instructions* nil))
    (let ((start (make-vop-label "start-"))
	  (make-cons (make-vop-label "make-cons-"))
	  (regs-args (make-vop-label "regs-args-"))
	  (rdx-label (make-vop-label "rdx-"))
	  (rdi-label (make-vop-label "rdi-"))
	  (r8-label (make-vop-label "r8-"))
	  (r9-label (make-vop-label "r9-"))
	  (zero-args-label (make-vop-label "zero-args-label-"))
	  (zero-rest-args-label (make-vop-label "zero-rest-args-label-"))
	  (set-cons-result (make-vop-label "set-cons-result-"))
	  (exit (make-vop-label "exit-")))

      ;; we are no longer storing fixnum in RCX
      ;; (inst :shr *fun-number-of-arguments-reg* *tag-size*)

      (inst :mov *tmp-reg* *fun-number-of-arguments-reg*)

      ;; handle case when there are no arguments
      (inst :test *fun-number-of-arguments-reg* *fun-number-of-arguments-reg*)
      (inst :jump-fixup :je zero-args-label)

      ;; FIXME, allocation is no good, fixed-arguments-count ?
      (inst :lea :r12 (@ *fun-number-of-arguments-reg* nil nil (- fixed-arguments-count)))
      (inst :test :r12 :r12)
      (inst :jump-fixup :je zero-rest-args-label)
      (inst :mov :r13 (@ *heap-header-reg*))
      (inst :mov :r14 :r13) ;; save header start,current allocation pointer is in R14
      (inst :add :r12 :r12)
      (inst :lea :r13 (@ :r13 :r12 *word-size* (* 2 *word-size*))) ;; 2 * *word-size* is for last fixed-arg CONS
      (inst :mov (@ *heap-header-reg*) :r13)
      (inst :mov :r12 *nil*) ;; first cdr is NIL

      ;; loop start
      ;; loop vars: r14, r12
      ;; r14 -> allocation pointer
      ;; r12 -> current CDR of &REST list (consing list from last argument)
      ;; *tmp-reg* -> stack argument offset
      (inst :label start)
      (inst :cmp *fun-number-of-arguments-reg* fixed-arguments-count)
      (inst :jump-fixup :je set-cons-result)
      (inst :cmp *fun-number-of-arguments-reg* (length *fun-arguments-regs*)) ; is number of args in register
      (inst :jump-fixup :jle regs-args)

      ;; FIXME, R13
      ;; stack arguments processing
      ;; (inst :lea :r13 (@ *fun-number-of-arguments-reg* nil nil (- (length *fun-arguments-regs*))))
      ;; ;; FIXME, stack arguments reverse order
      ;; (inst :mov :r11 (@ *base-pointer-reg* :r13 *word-size* *word-size*))
      (inst :mov :r13 *tmp-reg*)
      (inst :sub :r13 *fun-number-of-arguments-reg*)
      (inst :lea :r13 (@ nil :r13 *word-size* (* 2 *word-size*)))
      (inst :mov :r11 (@ *base-pointer-reg* :r13))
      
      (inst :jump-fixup :jmp make-cons)

      
      (inst :label regs-args)
      (inst :cmp *fun-number-of-arguments-reg* 4)
      (inst :jump-fixup :je r9-label)
      (inst :cmp *fun-number-of-arguments-reg* 3)
      (inst :jump-fixup :je r8-label)
      (inst :cmp *fun-number-of-arguments-reg* 2)
      (inst :jump-fixup :je rdi-label)

      (inst :label rdx-label)
      (inst :mov :r11 :rdx)
      (inst :jump-fixup :jmp make-cons)

      (inst :label rdi-label)
      (inst :mov :r11 :rdi)
      (inst :jump-fixup :jmp make-cons)

      (inst :label r8-label)
      (inst :mov :r11 :r8)
      (inst :jump-fixup :jmp make-cons)

      (inst :label r9-label)
      (inst :mov :r11 :r9)

      (inst :label make-cons)
      ;; expecting car in r11, cdr in r12, resulting cons is in r12
      (inst :mov (@ :r14) :r11)
      (inst :mov (@ :r14 nil nil *word-size*) :r12)
      (inst :lea :r12 (@ :r14 nil nil *list-tag*))
      (inst :lea :r14 (@ :r14 nil nil (* 2 *word-size*)))
      (inst :dec *fun-number-of-arguments-reg*)
      (inst :jump-fixup :jmp start)
      
      ;; we have fixed args but no rest args case
      (inst :label zero-rest-args-label)
      (inst :mov :r13 (@ *heap-header-reg*))
      (inst :mov :r14 :r13)
      (inst :lea :r13 (@ :r13 nil nil (* 2 *word-size*)))
      (inst :mov (@ *heap-header-reg*) :r13)
      (inst :mov :r12 *nil*)

      (inst :label set-cons-result)

      (cond ((and (> fixed-arguments-count 0)
		  (<= fixed-arguments-count (length *fun-arguments-regs*)))
	     (let ((register (nth (- fixed-arguments-count 1) *fun-arguments-regs*)))
	       (inst :mov (@ :r14) register)
	       (inst :mov (@ :r14 nil nil *word-size*) :r12)
	       (inst :lea register (@ :r14 nil nil *list-tag*))))
	    ((= fixed-arguments-count 0)
      	     (inst :mov :rdx :r12))
	    (t
	     (inst :mov :r11 *tmp-reg*)
	     (inst :sub :r11 *fun-number-of-arguments-reg*)
	     (inst :lea :r13 (@ nil :r11 *word-size* (* 2 *word-size*)))
	     (inst :mov :r11 (@ *base-pointer-reg* :r13))
	     (inst :mov (@ :r14) :r11)
	     (inst :mov (@ :r14 nil nil *word-size*) :r12)

	     ;; we are setting last fixed parameter to (LAST_FIXED_PARAMETER . &REST)
	     (inst :lea :r11 (@ :r14 nil nil *list-tag*))
	     (inst :mov (@ *base-pointer-reg* :r13) :r11)

	     ;; since compiler doesn't know how many arguments will be at runtime
	     ;; we calculate RBP offset for first stack argument
	     ;; compiler count on this to be able to grab stack arguments
	     ;; when there is &REST in arguments list
	     (inst :mov *fun-number-of-arguments-reg* *tmp-reg*)
	     (inst :sub *fun-number-of-arguments-reg* (+ 1 (length *fun-arguments-regs*)))
	     (inst :lea *fun-number-of-arguments-reg* (@ nil *fun-number-of-arguments-reg* *word-size* (* 2 *word-size*)))))
      
      (inst :jump-fixup :jmp exit)
      
      (inst :label zero-args-label)
      (inst :mov :rdx *nil*) 
      
      (inst :label exit))
    (reverse *segment-instructions*)))

;;; NOTE
;;; This is executed after every call and it does have branch
;;; but in 99.9% of cases onethe same branch is taken so predictor will always be right
(defun maybe-mv-adjust-stack-generator ()
  (let ((*segment-instructions* nil)
	(skip-align (make-vop-label "skip-align"))
	(skip (make-vop-label "skip-stack-adjust-")))
    (inst :cmp *fun-number-of-arguments-reg* (length *fun-arguments-regs*))
    (inst :jle skip)
    (inst :lea *tmp-reg* (@ *fun-number-of-arguments-reg* nil nil (- (length *fun-arguments-regs*))))
    (inst :test *tmp-reg* 1)
    (inst :jz skip-align )
    (inst :inc *tmp-reg*)
    (inst :label skip-align)
    (inst :shl *tmp-reg* 3)
    (inst :add *stack-pointer-reg* *tmp-reg*)
    (inst :label skip)
    (reverse *segment-instructions*)))


;;; looks bloated
;;; anyway, we can optimize  most of leaf function calls to tail calls
;;; FIXME, extract this to assembly stub, emiting this at every tail call is stupid, it will increase binary
(defun maybe-copy-mv-stack-frame-and-return-generator (function-frame-size)
  (let ((*segment-instructions* nil)
	(copy-loop (make-vop-label "stack-copy-loop-"))
	(skip-copy-loop (make-vop-label "skip-copy-loop-"))
	(skip-alignment (make-vop-label "skip-alignment-") )
	(copy-to-caller-frame (make-vop-label "copy-to-caller-frame")))

    ;; check if we have extra values on stack
    (inst :mov *tmp-reg* *fun-number-of-arguments-reg*)
    (inst :sub *tmp-reg* (length *fun-arguments-regs*))
    (inst :jg copy-to-caller-frame)

    (when (> function-frame-size 0 )
      (inst :add *stack-pointer-reg* (* function-frame-size *word-size*)))

    (add-instructions (generate-function-epilogue))

    (inst :ret)

    (inst :label copy-to-caller-frame)

    ;; this is almost the same sequence as in EMIT-VALUES-NODE-SSA
    ;; first restore caller registers because we will overwrite it with value copying
    (let ((index 1))
      (dolist (reg (reverse *preserved-regs*))
	(inst :mov reg (@ *base-pointer-reg* nil nil (- (* index clcomp::*word-size*))))
	(incf index)))
    ;; save caller RBP
    (inst :mov *tmp-reg-2* (@ *base-pointer-reg*))
    (inst :push *tmp-reg-2*)
    ;; save RIP
    (inst :mov *tmp-reg-2* (@ *base-pointer-reg*
			      nil
			      nil
			      clcomp::*word-size*))
    (inst :push *tmp-reg-2*)

    ;; pfff, we need another two registers
    (inst :push *fun-number-of-arguments-reg*)
    (inst :push :R11)

    ;; *tmp-reg* -> remaining number of stack values
    ;; *tmp-reg-2* -> copy destination offset
    ;; *fun-number-of-arguments-reg* -> copy source offset

    ;; we are copying from last value
    ;; source last value is now at rsp+32 (4 pushes),
    ;; we need to calculate destination last value offset, first calculate first value offset
    ;; need to include alignment into calculation of first value offset, align slot is always at the top
    (inst :mov *tmp-reg-2* clcomp::*word-size*)
    (inst :test *tmp-reg* 1)
    (inst :jz skip-alignment)
    (inst :mov *tmp-reg-2* 0) ;; alignment slot is at [RBP+8] so our start is [RBP],
    (inst :mov (@ *base-pointer-reg* nil nil clcomp::*word-size*) *nil*) ;; set alignment slot to NIL (GC friendly)

    (inst :label skip-alignment)

    ;; calculate source last value offset
    (inst :lea *fun-number-of-arguments-reg* (@ *tmp-reg* nil nil -1))
    (inst :shl *fun-number-of-arguments-reg* 3)
    (inst :sub *tmp-reg-2* *fun-number-of-arguments-reg*) ;; slot where we start copying to

    (inst :mov *fun-number-of-arguments-reg* 32) ;; start source slot (we did 4 pushes)
    (inst :label copy-loop)
    (inst :mov :R11 (@ *stack-pointer-reg* *fun-number-of-arguments-reg*))
    (inst :mov (@ *base-pointer-reg* *tmp-reg-2*) :R11)
    (inst :dec *tmp-reg*)
    (inst :jz skip-copy-loop)
    ;; both source and destination slot goes upward the stack
    (inst :add *tmp-reg-2* clcomp::*word-size*)
    (inst :add *fun-number-of-arguments-reg* clcomp::*word-size*)
    (inst :jmp copy-loop)
    
    (inst :label skip-copy-loop)

    ;; restore caller RBP and RIP
    ;; rewind stack and return
    (inst :pop :R11)
    (inst :pop *fun-number-of-arguments-reg*)

    ;; calculate where is new RSP
    ;; here we now that *tmp-reg-2* has the offset of first value (not the alignment slot)
    (inst :lea *tmp-reg* (@ *fun-number-of-arguments-reg* nil nil (- (1+ (length *fun-arguments-regs*)))))
    (inst :shl *tmp-reg* 3)
    (inst :sub *tmp-reg-2* *tmp-reg*)
    (inst :lea *tmp-reg* (@ *base-pointer-reg* *tmp-reg-2*))


    ;; pop RIP
    (inst :pop *tmp-reg-2*)

    ;; set caller frame pointer, we don't need it anymore
    (inst :pop *base-pointer-reg*)

    ;; set new stack pointer
    (inst :mov *stack-pointer-reg* *tmp-reg*)

    ;; put back RIP at the end of stack pointer
    (inst :push *tmp-reg-2*)
    (inst :ret)

    (reverse *segment-instructions*)))

(defun mvb-value-stack-offset (index places-count)
  (let ((diff (- places-count (length *fun-arguments-regs*)))
	(index-diff (1+ (- index (length *fun-arguments-regs*)))))
    (* (- (- index-diff diff)) *word-size*)))

(defun multiple-value-bind-generator (places)
  (let ((*segment-instructions* nil))
    (let ((places-count (length places))
	  (end-label (make-vop-label "end-label-"))
	  (nil-labels (mapcar (lambda (p)
				(declare (ignore p))
				(make-vop-label "nil-label-"))
			      places)))
      (do ((places places (cdr places))
	   (lbs nil-labels (cdr lbs))
	   (regs *fun-arguments-regs* (cdr regs))
	   (index 0 (+ index 1)))
	  ((null places))
	(let ((place (car places))
	      (reg (car regs)))
	  (inst :cmp *fun-number-of-arguments-reg*  index)
	  (inst :jump-fixup :je (car lbs))
	  (if reg
	      (inst :mov place reg)
	      (if (is-register place)
		  (inst :mov place reg (@ *stack-pointer-reg* (mvb-value-stack-offset index places-count)))
		  (progn
		    (inst :mov *tmp-reg* (@ *stack-pointer-reg* (mvb-value-stack-offset index places-count)))
		    (inst :mov place *tmp-reg*))))))
      (inst :jump-fixup :jmp :end-label)
      (do ((nil-labels nil-labels (cdr nil-labels))
	   (places places (cdr places)))
	  ((null nil-labels) nil)
	(inst :label (car nil-labels) )
	(inst :mov (car places) *nil*))
      (inst :label end-label))
    (reverse *segment-instructions*)))
