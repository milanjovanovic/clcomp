(in-package :clcomp)

(define-vop char-code (res :register) ((arg1 :register))
  (inst :mov res arg1))
