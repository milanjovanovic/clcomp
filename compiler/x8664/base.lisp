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


(define-vop bla (res :register) ((arg :register))
  (inst :ud2))
