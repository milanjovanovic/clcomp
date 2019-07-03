(in-package :clcomp)

(define-vop + (res :register) ((arg1 :register) (arg2 :register))
  (inst :mov res arg1)
  (inst :add res arg2))

(define-vop - (res :register) ((arg1 :register) (arg2 :register))
  (inst :mov res arg1)
  (inst :sub res arg2))

(define-vop > (res :register) ((arg1 :register) (arg2 :register))
  (inst :cmp arg1 arg2)
  )

(define-vop < (res :register) (arg1 :register) (arg2 :register)
  (inst :cmp arg1 arg2))

(define-vop = (res :register) ((arg1 :register) (arg2 :register))
  (inst :cmp arg1 arg2)
  (inst :jump-fixup :je :equal)
  (inst :mov res *nil*)
  (inst :jump-fixup :jmp :end)
  (inst :label :equal)
  (inst :mov res *t*)
  (inst :label :end))
