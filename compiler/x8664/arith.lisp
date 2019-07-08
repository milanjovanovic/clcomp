(in-package :clcomp)

(define-vop + (res :register) ((arg1 :register) (arg2 :register))
  (inst :mov res arg1)
  (inst :add res arg2))

(define-vop - (res :register) ((arg1 :register) (arg2 :register))
  (inst :mov res arg1)
  (inst :sub res arg2))

(define-vop > (res :register) ((arg1 :register) (arg2 :register))
  (let ((less-label (make-vop-label "less"))
	(end-label (make-vop-label "end")))
   (inst :cmp arg1 arg2)
   (inst :jump-fixup :jl less-label)
   (inst :mov res *nil*)
   (inst :jump-fixup :jmp end-label)
   (inst :label less-label)
   (inst :mov res *t*)
   (inst :label end-label)))

(define-vop < (res :register) ((arg1 :register) (arg2 :register))
  (let ((greater-label (make-vop-label "greater"))
	(end-label (make-vop-label "end")))
   (inst :cmp arg1 arg2)
   (inst :jump-fixup :jl greater-label)
   (inst :mov res *nil*)
   (inst :jump-fixup :jmp end-label)
   (inst :label greater-label)
   (inst :mov res *t*)
   (inst :label end-label)))

(define-vop = (res :register) ((arg1 :register) (arg2 :register))
  (let ((equal-label (make-vop-label "equal"))
	(end-label (make-vop-label "end")))
   (inst :cmp arg1 arg2)
   (inst :jump-fixup :jg equal-label)
   (inst :mov res *nil*)
   (inst :jump-fixup :jmp end-label)
   (inst :label equal-label)
   (inst :mov res *t*)
   (inst :label end-label)))
