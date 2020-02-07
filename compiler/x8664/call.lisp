(in-package :clcomp)

(define-vop %error (res :register) ((msg :register))
  (inst :mov :RAX 33560272)
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


(define-vop debug (res :register) ((msg :register))
  (inst :mov :RAX 33559104)
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

;; (define-vop apply (res :register) ((fun :register) (arguments :register))
;;   ;; FIXME
;;   (inst :mov *fun-address-reg* fun)
;;   (inst :call *fun-address-reg*))

(defun delistify-code-generator ()
  ;; for apply use only, FIXME
  (let ((*segment-instructions* nil))
    (reverse *segment-instructions*)))

(defun listify-code-generator (fixed-arguments-count)
  (print (list 'fixed-arguments-count fixed-arguments-count))
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

      ;; handle case when there are no arguments
      (inst :test *fun-number-of-arguments-reg* *fun-number-of-arguments-reg*)
      (inst :jump-fixup :je zero-args-label)

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
      (inst :label start)
      (inst :cmp *fun-number-of-arguments-reg* fixed-arguments-count)
      (inst :jump-fixup :je set-cons-result)
      (inst :cmp *fun-number-of-arguments-reg* (length *fun-arguments-regs*)) ; is number of args in register
      (inst :jump-fixup :jle regs-args)

      ;; FIXME, R13
      ;; stack arguments processing
      (inst :lea :r13 (@ *fun-number-of-arguments-reg* nil nil (- (length *fun-arguments-regs*))))
      (inst :mov :r11 (@ *base-pointer-reg* :r13 *word-size* *word-size*))
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
      (inst :lea :r13 (@ :r13 nil nil (* 2 *word-size*))) ;; 2 * *word-size* is for last fixed-arg CONS
      (inst :mov (@ *heap-header-reg*) :r13)
      (inst :mov :r12 *nil*)

      (inst :label set-cons-result)

      (cond ((and (> fixed-arguments-count 0)
		  (< fixed-arguments-count 5))
	     (let ((register (nth (- fixed-arguments-count 1) *fun-arguments-regs*)))
	       (inst :mov (@ :r14) register)
	       (inst :mov (@ :r14 nil nil *word-size*) :r12)
	       (inst :lea register (@ :r14 nil nil *list-tag*))))
	    ((= fixed-arguments-count 0)
      	     (inst :mov :rdx :r12))
	    (t
	     (inst :mov *tmp-reg* (@ *base-pointer-reg* nil nil (* *word-size* (+ 1 (- fixed-arguments-count
										       (length *fun-arguments-regs*))))))
	     (inst :mov (@ :r14) *tmp-reg*)
	     (inst :mov (@ :r14 nil nil *word-size*) :r12)
	     (inst :lea *tmp-reg* (@ :r14 nil nil *list-tag*))
	     (inst :mov (@ *base-pointer-reg* nil nil (* *word-size* (+ 1 (- fixed-arguments-count
									     (length *fun-arguments-regs*)))))
		   *tmp-reg*)))
      (inst :jump-fixup :jmp exit)
      
      (inst :label zero-args-label)
      (inst :mov :rdx *nil*)
      
      (inst :label exit))
    (reverse *segment-instructions*)))

  ;; (define-vop open (:res register) ((path :register) (mode :register)))

  ;; (define-vop close (:res register) ())

  ;; (define-vop read (:res register) ())

  ;; (define-vop write (:res register) ((buf :register) (nbyte :register)))
