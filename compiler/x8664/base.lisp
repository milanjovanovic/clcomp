(in-package :clcomp)

(define-vop %raw (res :register) ((arg :register))
  (inst :mov res arg)
  (inst :and res (ash -1 3)))

(define-vop eq (res :register :stack) ((arg1 :register :stack) (arg2 :register :stack))
  (let ((true-label (make-vop-label "true"))
	(exit-label (make-vop-label "exit")))
    ;; FIXME, we need way to auto do stuff like this
    (inst :mov *tmp-reg* arg1)
    (inst :cmp *tmp-reg* arg2)
    (inst :jump-fixup :je true-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label true-label)
    (inst :mov res *t*)
    (inst :label exit-label)))

;; size = number of qwords
(define-vop allocate (res :register) ((size :immediate :register :stack))
  (inst :mov res (@ *heap-header-reg*))
  (inst :mov *tmp-reg* size)
  (inst :lea *tmp-reg* (@ res *tmp-reg* *word-size*))
  (inst :mov (@ *heap-header-reg*) *tmp-reg*))

;;; closures support
(define-vop make-closure-env (res :register) ((count :immediate))
  (inline-vop 'allocate res (1+ count) $stack-top-operand$)
  (inst :mov (@ res) (get-extended-tag 'closure-env))
  (inst :add res *pointer-tag*))

(define-vop get-from-closure-env (res :register :stack) ((env :register :stack)
							 (index :immediate))
  (inst :mov *tmp-reg* env)
  ;; get bcell
  (inst :mov *tmp-reg* (@ *tmp-reg* nil nil (- (+ (* 1 *word-size*) ;; header type qword
						  (* index *word-size*))
					       *pointer-tag*)))
  ;; get value
  (inst :mov *tmp-reg* (@ *tmp-reg* nil nil (- (* 1 *word-size*) ;; header type qword
					       *pointer-tag*)))
  (inst :mov res *tmp-reg*))

(define-vop make-bcell (res :register) ((value :register))
  (inline-vop 'allocate res 2 $stack-top-operand$)
  (inst :mov (@ res) (get-extended-tag 'bcell))
  (inst :mov (@ res nil nil *word-size*) value)
  (inst :add res *pointer-tag*))

(define-vop get-bcell (res :register) ((env :register)
				       (index :immediate))
  (inst :mov res (@ env nil nil index)))

(define-vop get-bcell-value (res :register) ((env :register)
					     (index :immediate))
  (inline-vop 'get-bcell res env index)
  (inst :mov res (@ res)))

(define-vop bla (res :register) ((arg :register))
  (inst :ud2))
