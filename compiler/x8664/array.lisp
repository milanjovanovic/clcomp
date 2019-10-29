(in-package :clcomp)

(define-vop allocate-array (res :register) ((arg :register) (tag :register)) 
  (inst :mov *tmp-reg* (@ *heap-header-reg*))
  (inst :mov res *tmp-reg*)
  (inst :shr arg *tag-size*)
  (inst :lea *tmp-reg* (@ *tmp-reg* arg *word-size* (* *word-size* *array-header-size*)))
  (inst :mov (@ *heap-header-reg*) *tmp-reg*)
  (inst :mov (@ res nil nil nil) tag)
  (inst :shl arg *tag-size*)
  (inst :mov (@ res nil nil *word-size*) arg)
  (inst :add res *pointer-tag*))

(define-vop aref (res :register) ((array :register)
				  (index :register))
  (inst :lea res (@ array nil nil (- *pointer-tag*)))
  (inst :mov *tmp-reg* index)
  (inst :shr *tmp-reg* *tag-size*)
  (inst :mov res (@ res *tmp-reg* *word-size* (* 2 *word-size*))))

(define-vop setf-aref (res :register) ((array :register)
				       (index :register)
				       (value :register))
  (inst :lea res (@ array nil nil (- *pointer-tag*)))
  (inst :mov *tmp-reg* index)
  (inst :shr *tmp-reg* *tag-size*)
  (inst :mov (@ res *tmp-reg* *word-size* (* 2 *word-size*)) value)
  (inst :mov res value))

(define-vop array-total-size (res :register) ((array :register))
  (inst :mov res (@ array nil nil (- *word-size* *pointer-tag*))))

(define-vop pointerp (res :register) ((arg :register))
  (let ((is-pointer-label (make-vop-label "is-pointer"))
	(exit-label (make-vop-label "exit")))
    (inst :lea *tmp-reg* (@ arg nil nil (- *pointer-tag*)))
    (inst :test *tmp-reg* *pointer-tag*)
    (inst :jump-fixup :je is-pointer-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label is-pointer-label)
    (inst :mov res *t*)
    (inst :label exit-label)))

;;; FIXME,  this is WRONG
;;; array should have distinct tags, implement lowtag for all array types pointers
(define-vop arrayp (res :register) ((arg :register))
  (let ((is-array (make-vop-label "is-array"))
	(exit-label (make-vop-label "exit")))
    (inst :lea *tmp-reg* (@ arg nil nil (- *pointer-tag*)))
    (inst :test *tmp-reg* *pointer-tag*)
    (inst :jump-fixup :je is-array)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label is-array)
    (inst :mov res *t*)
    (inst :label exit-label)))
