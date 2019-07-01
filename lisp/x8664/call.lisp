(in-package :clcomp)

;; FIXME, no return register, no 
(define-vop c_print_lisp (res :register) ((arg :register))
  (inst :push :R15)
  (when (not (eq arg :RDI))
    (inst :mov :RDI arg))
  (inst :mov :RAX 33560208)
  ;;  (inst :sub :RSP 8)
  (inst :call :RAX)
  (inst :mov res :RAX)
  ;;  (inst :add :RSP 8)
  (inst :pop :R15))
