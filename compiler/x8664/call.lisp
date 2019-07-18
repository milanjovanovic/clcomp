(in-package :clcomp)


(define-vop c_print_lisp (res :register) ((arg :register))
  (when (not (eq arg :RDI))
    (inst :mov :RDI arg))
  (inst :mov :RAX 33560208)
  (inst :push :R15)
  (inst :push :RBP)
  (inst :mov :RSP :RBP)
  (inst :call :RAX)
  (inst :pop :RBP)
  (inst :pop :R15)
  (inst :mov res :RAX))


#+nil
 (define-vop foo (res :register) ((arg :register))
   (when (not (eq arg :RDI))
     (inst :mov :RDI arg))
   (inst :mov :RAX 33561792)
   (inst :push :R15)
   (inst :sub :RSP 8)
   (inst :call :RAX)
   (inst :add :RSP 8)
   (inst :pop :R15)
   (inst :mov :RBX :RAX))
