(in-package :clcomp)

(define-vop char-code (res :register) ((arg1 :register))
  (inst :lea res (@ arg1 nil nil (- *char-tag*))))

(define-vop code-char (res :register) ((arg1 :register))
  (inst :lea res (@ arg1 nil nil *char-tag*)))

(define-vop characterp (res :register) ((arg :register))
  (let ((true-label (make-vop-label "true"))
	(exit-label (make-vop-label "exit")))
    (inst :lea *tmp-reg* (@ arg nil nil (- *char-tag*)))
    (inst :test *tmp-reg* *mask*)
    (inst :jump-fixup :je true-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label true-label)
    (inst :mov res *t*)
    (inst :label exit-label)))
