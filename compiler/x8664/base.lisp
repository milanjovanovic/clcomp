(in-package :clcomp)

(define-vop %raw (res :register) ((arg :register))
  (inst :mov res arg)
  (inst :and res (ash -1 3)))

(define-vop eq (res :register :stack) ((arg1 :register :stack) (arg2 :register :stack))
  (let ((true-label (make-vop-label "true"))
	(exit-label (make-vop-label "exit")))
    ;; FIXME, we need way to auto do stuff like this
    (mov *tmp-reg* arg1)
    (inst :cmp *tmp-reg* arg2)
    (inst :jump-fixup :je true-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label true-label)
    (inst :mov res *t*)
    (inst :label exit-label)))

;; FIXME, allocate should also accept jump location in a case of not enough space for allocation
(define-vop allocate (res :register) ((size :immediate :register :stack))
  (inst :mov res (@ *heap-header-reg*))
  (inst :mov *tmp-reg* size)
  (inst :lea *tmp-reg* (@ res *tmp-reg* *word-size*))
  (inst :mov (@ *heap-header-reg*) *tmp-reg*))

(define-vop bla (res :register) ((arg :register))
  (inst :ud2))
