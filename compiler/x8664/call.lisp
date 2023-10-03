(in-package :clcomp)

(define-vop %set-rip-value (res :register) ((rip-location :register) (lambda :register))
  ;; FIXME
  )

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
    
    (inst :shl *fun-number-of-arguments-reg* *tag-size*)
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

      (inst :shr *fun-number-of-arguments-reg* *tag-size*)

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


;;; FIXME, maybe we want to do this with ordinary lisp code and TAGBODY/GO
(defun multiple-value-bind-generator (&rest places)
  (declare (ignore places))
  (let ((*segment-instructions* nil))
    (let ((end-label (make-vop-label "end-label-"))
	  (register-arguments-labels (mapcar (lambda (reg)
					       (make-vop-label (string reg)))
					     *fun-arguments-regs*)))

      (do ((regs *fun-arguments-regs* (cdr regs))
	   (reg-labels register-arguments-labels (cdr reg-labels)))
	  ((null regs))
	(inst :label (car reg-labels))
	(inst :mov (car regs) *nil*))
      (inst :label end-label))
    (reverse *segment-instructions*)))

