(in-package :clcomp)

(define-vop %error (res :register) ((msg :register))
  (inst :mov :RAX 33560352)
  (dolist (reg *c-call-save-registers*)
    (unless (eq reg res)
      (inst :push reg)))
  (inst :push :rbp)
  (inst :mov :rbp :rsp)
  (when (not (eq msg :rdi))
    (inst :mov :rdi msg))
  (inst :call :rax)
  (inst :mov res :RAX)
  (inst :pop :rbp)
  (dolist (reg (reverse *c-call-save-registers*))
    (unless (eq reg res)
      (inst :pop reg))))

(define-vop from-lisp-test (res :register) ()
  (inst :mov :RAX 33560352)
  (dolist (reg *c-call-save-registers*)
    (unless (eq reg res)
      (inst :push reg)))
  (inst :push :rbp)
  (inst :mov :rbp :rsp)
  (inst :call :rax)
  (inst :mov res :RAX)
  (inst :pop :rbp)
  (dolist (reg (reverse *c-call-save-registers*))
    (unless (eq reg res)
      (inst :pop reg))))

(defun listify-code-generator (fixed-arguments-count)
  (let ((*segment-instructions* nil))
    (let ((start (make-vop-label "start-"))
	  (make-cons (make-vop-label "make-cons-"))
	  (regs-args (make-vop-label "regs-args-"))
	  (rdx-label (make-vop-label "rdx-"))
	  (rdi-label (make-vop-label "rdi-"))
	  (r8-label (make-vop-label "r8-"))
	  (r9-label (make-vop-label "r9-"))
	  (zero-args (make-vop-label "zero-args-"))
	  (exit (make-vop-label "exit-")))

      (inst :shr *fun-number-of-arguments-reg* *tag-size*)

      ;; handle case when there are no arguments
      (inst :test *fun-number-of-arguments-reg* *fun-number-of-arguments-reg*)
      (inst :jump-fixup :je zero-args)

      ;; allocate space for whole list
      (inst :mov :r12 *fun-number-of-arguments-reg*)
      (inst :sub :r12 fixed-arguments-count)
      (inst :mov :r13 (@ *heap-header-reg*))
      (inst :mov :r14 :r13) ;; save header start, current allocation pointer is in R14
      (inst :add :r12 :r12)
      (inst :lea :r13 (@ :r13 :r12 8))
      (inst :mov (@ *heap-header-reg*) :r13)
      (inst :mov :r12 *nil*) ;; first cdr is NIL

      ;; loop start
      ;; loop vars: r14, r12
      (inst :label start)
      (inst :cmp *fun-number-of-arguments-reg* fixed-arguments-count)
      (inst :jump-fixup :je exit)
      (inst :cmp *fun-number-of-arguments-reg* 4) ; is number of args in register
      (inst :jump-fixup :jle regs-args)

      ;; FIXME, R13
      ;; stack arguments processing
      (inst :lea :r13 (@ *fun-number-of-arguments-reg* nil nil (- 4)))
      (inst :mov :r11 (@ *base-pointer-reg* :r13 8 8))
      ;; save address of current cons in a case that this is our last &rest arg
      (inst :lea *tmp-reg* (@ :r14 nil nil *list-tag*))
      (inst :mov (@ *base-pointer-reg* :r13 8 8) *tmp-reg*)
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
      ;; save address of current cons in a case that this is our lats &rest arg
      (inst :lea :rdx (@ :r14 nil nil *list-tag*))
      (inst :jump-fixup :jmp make-cons)

      (inst :label rdi-label)
      (inst :mov :r11 :rdi)
      (inst :lea :rdi (@ :r14 nil nil *list-tag*))
      (inst :jump-fixup :jmp make-cons)

      (inst :label r8-label)
      (inst :mov :r11 :r8)
      (inst :lea :r8 (@ :r14 nil nil *list-tag*))
      (inst :jump-fixup :jmp make-cons)

      (inst :label r9-label)
      (inst :mov :r11 :r9)
      (inst :lea :r9 (@ :r14 nil nil *list-tag*))
      (inst :jump-fixup :jmp make-cons)

      (inst :label make-cons)
      ;; expecting car in r11, cdr in r12, resulting cons is in r12
      (inst :mov (@ :r14) :r11)
      (inst :mov (@ :r14 nil nil 8) :r12)
      (inst :lea :r12 (@ :r14 nil nil *list-tag*))
      (inst :lea :r14 (@ :r14 nil nil (* 2 *word-size*)))
      (inst :dec *fun-number-of-arguments-reg*)
      (inst :jump-fixup :jmp start)

      (inst :label zero-args)
      (inst :mov :rdx *nil*)
    
      (inst :label exit))
    (reverse *segment-instructions*)))

  ;; (define-vop open (:res register) ((path :register) (mode :register)))

  ;; (define-vop close (:res register) ())

  ;; (define-vop read (:res register) ())

  ;; (define-vop write (:res register) ((buf :register) (nbyte :register)))
