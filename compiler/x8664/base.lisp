(in-package :clcomp)

(define-vop %raw (res :register) ((arg :register))
  (inst :mov res arg)
  (inst :and res (ash -1 3)))

(define-vop eq (res :register) ((arg1 :register) (arg2 :register))
  (let ((true-label (make-vop-label "true"))
	(exit-label (make-vop-label "exit")))
    (inst :cmp arg1 arg2)
    (inst :jump-fixup :je true-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label true-label)
    (inst :mov res *t*)
    (inst :label exit-label)))

;; size = number of qwords
;; FIXME, not good
(define-vop allocate (res :register) ((size :immediate :register))
  (inst :mov res (@ *heap-header-reg*))
  (inst :mov *tmp-reg* size)p
  (inst :lea res (@ res nil *tmp-reg* *word-size*))
  (inst :mov (@ *heap-header-reg*) res)
  ;; FIXME, this LEA is wrong
  (inst :lea res (@ res nil nil (- (* 2 *word-size*)))))


;;; closures support
(define-vop make-closure-env (res :register) ((count :immediate))
  (inline-vop 'allocate res (1+ count) $stack-top-operand$)
  (inst :mov (@ res) value))

;; FIXME
;; FIXME, not good
(define-vop make-bcell (res :register) ((value :register))
  (inline-vop 'allocate res 1 $stack-top-operand$)
  (inst :mov (@ res) value))

(define-vop get-bcell (res :register) ((env :register)
				       (index :immediate))
  (inst :mov res (@ env nil nil index)))

(define-vop get-bcell-value (res :register) ((env :register)
					     (index :immediate))
  (inline-vop 'get-bcell res env index)
  (inst :mov res (@ res)))

(define-vop bla (res :register) ((arg :register))
  (inst :ud2))
