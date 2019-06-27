(in-package :clcomp)

(define-vop + (res :register) ((arg1 :register) (arg2 :register))
  (inst :mov res arg1)
  (inst :add res arg2))
