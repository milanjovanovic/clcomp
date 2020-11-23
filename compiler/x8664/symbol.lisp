(in-package :clcomp)

(define-vop %get-env (res :register) ()
  (inst :mov res (@ *heap-header-reg* nil nil (* 8 3))))

(define-vop %set-env (res :register) ((arg :register))
  (inst :mov (@ *heap-header-reg* nil nil (* 8 3)) arg)
  (inst :mov res arg))

(define-vop make-symbol (res :register) ((name :register))
  (inst :mov *tmp-reg* (@ *heap-header-reg*))
  (inst :mov res *tmp-reg*)
  (inst :add *tmp-reg* (* 5 *word-size*))
  (inst :mov (@ *heap-header-reg*) *tmp-reg*)
  (inst :mov (@ res nil nil nil) name)
  (inst :mov (@ res nil nil *word-size*) *nil*)
  (inst :mov (@ res nil nil (* 2 *word-size*)) *nil*)
  (inst :mov (@ res nil nil  (* 3 *word-size*)) *nil*)
  (inst :mov (@ res nil nil  (* 4 *word-size*)) *nil*)
  (inst :add res *symbol-tag*))

(define-vop symbol-name (res :register) ((symbol :register))
  (inst :mov res (@ symbol nil nil (- *symbol-tag*))))

(define-vop symbol-value (res :register) ((symbol :register))
  (inst :mov res (@ symbol nil nil (- *word-size* *symbol-tag*))))

(define-vop set-symbol-value (res :register) ((symbol :register) (value :register))
  (inst :mov (@ symbol nil nil (- *word-size* *symbol-tag*)) value)
  (inst :mov res value))

(define-vop symbol-function (res :register) ((symbol :register))
  (inst :mov res (@ symbol nil nil (- (* 2 *word-size*) *symbol-tag*))))

(define-vop symbol-plist (res :register) ((symbol :register))
  (inst :mov res (@ symbol nil nil (- (* 3 *word-size*) *symbol-tag*))))

(define-vop symbol-package (res :register) ((symbol :register))
  (inst :mov res (@ symbol nil nil (- (* 4 *word-size*) *symbol-tag*))))

(define-vop %set-symbol-package (res :register) ((symbol :register) (value :register))
  (inst :mov (@ symbol nil nil (- (* 4 *word-size*) *symbol-tag*)) value)
  (inst :mov res value))

(define-vop symbolp (res :register) ((symbol :register))
  (let ((true-label (make-vop-label "true-label"))
	(exit-label (make-vop-label "exit-label")))
    (inst :lea *tmp-reg* (@ symbol nil nil (- *symbol-tag*)))
    (inst :test *tmp-reg* *mask*)
    (inst :jump-fixup :je true-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label true-label)
    (inst :mov res *t*)
    (inst :label exit-label)))
