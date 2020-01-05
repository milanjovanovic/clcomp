(in-package :clcomp)

(define-vop allocate-array (res :register) ((arg :register)) 
  (inst :mov *tmp-reg* (@ *heap-header-reg*))
  (inst :mov res *tmp-reg*)
  (inst :shr arg *tag-size*)
  (inst :lea *tmp-reg* (@ *tmp-reg* arg *word-size* (* *word-size* *array-header-size*)))
  (inst :mov (@ *heap-header-reg*) *tmp-reg*)
  (inst :shl arg *tag-size*)
  (inst :mov (@ res nil nil *word-size*) arg)
  (inst :add res *pointer-tag*))

(define-vop aref (res :register) ((array :register)
				  (index :register))
  (inst :lea res (@ array nil nil (- *pointer-tag*)))
  (inst :mov *tmp-reg* index)
  (inst :shr *tmp-reg* *tag-size*)
  (inst :mov res (@ res *tmp-reg* *word-size* (* *array-header-size* *word-size*))))

(define-vop setf-aref (res :register) ((array :register)
				       (index :register)
				       (value :register))
  (inst :lea res (@ array nil nil (- *pointer-tag*)))
  (inst :mov *tmp-reg* index)
  (inst :shr *tmp-reg* *tag-size*)
  (inst :mov (@ res *tmp-reg* *word-size* (* *array-header-size* *word-size*)) value)
  (inst :mov res value))

(define-vop %array-tag (res :register) ((array :register))
  (inst :mov res (@ array nil nil (- *pointer-tag*))))

(define-vop %set-array-tag (res :register) ((array :register) (tag :register))
  (inst :mov (@ array nil nil (- *pointer-tag*)) tag)
  (inst :mov res tag))

(define-vop array-total-size (res :register) ((array :register))
  (inst :mov res (@ array nil nil (- *word-size* *pointer-tag*))))

(define-vop %array-type (res :register) ((array :register))
  (inst :lea res (@ array nil nil (- *pointer-tag*)))
  (inst :mov res (@ res nil nil (* 2 *word-size*))))

(define-vop %set-array-type (res :register) ((array :register) (type :register))
  (inst :lea res (@ array nil nil (- *pointer-tag*)))
  (inst :mov (@ res nil nil (* 2 *word-size*)) type)
  (inst :mov res type))

(define-vop %array-element-type (res :register) ((array :register))
  (inst :lea res (@ array nil nil (- *pointer-tag*)))
  (inst :mov res (@ res nil nil (* 3 *word-size*))))

(define-vop %set-array-element-type (res :register) ((array :register) (etype :register))
  (inst :lea res (@ array nil nil (- *pointer-tag*)))
  (inst :mov (@ res nil nil (* 3 *word-size*)) etype)
  (inst :mov res etype))

(define-vop %pointerp (res :register) ((arg :register))
  (let ((is-pointer-label (make-vop-label "is-pointer"))
	(exit-label (make-vop-label "exit")))
    (inst :lea *tmp-reg* (@ arg nil nil (- *pointer-tag*)))
    (inst :test *tmp-reg* *mask*)
    (inst :jump-fixup :je is-pointer-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label is-pointer-label)
    (inst :mov res *t*)
    (inst :label exit-label)))

(define-vop arrayp (res :register) ((arg :register))
  (let ((is-pointer-label (make-vop-label "is-pointer"))
	(is-array-label (make-vop-label "is-array"))
	(exit-label (make-vop-label "exit")))
    (inst :lea *tmp-reg* (@ arg nil nil (- *pointer-tag*)))
    (inst :test *tmp-reg* *mask*)
    (inst :jump-fixup :je is-pointer-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label is-pointer-label)
    (inst :cmp (@ :byte arg nil nil (- *pointer-tag*)) (get-extended-tag 'simple-array))
    (inst :jump-fixup :jnb is-array-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label is-array-label)
    (inst :mov res *t*)
    (inst :label exit-label)))

(define-vop stringp (res :register) ((arg :register))
  (let ((is-pointer-label (make-vop-label "is-pointer"))
	(is-string-label (make-vop-label "is-string"))
	(exit-label (make-vop-label "exit")))
    (inst :lea *tmp-reg* (@ arg nil nil (- *pointer-tag*)))
    (inst :test *tmp-reg* *mask*)
    (inst :jump-fixup :je is-pointer-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label is-pointer-label)
    (inst :cmp (@ :byte arg nil nil (- *pointer-tag*)) (get-extended-tag 'string))
    (inst :jump-fixup :je is-string-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label is-string-label)
    (inst :mov res *t*)
    (inst :label exit-label)))

(define-vop %copy-array (res :register) ((array :register))
  (let ((copy-loop-label (make-vop-label "copy-loop-label"))
	(copy-done-label (make-vop-label "copy-done-label")))
    (inst :mov *tmp-reg* (@ *heap-header-reg*))
    (inst :mov res *tmp-reg*)
    (inst :mov *fun-number-of-arguments-reg* (@ array nil nil (- *word-size* *pointer-tag*)))
    (inst :shr *fun-number-of-arguments-reg* *tag-size*)
    ;; bump allocation pointer
    (inst :lea *tmp-reg* (@ *tmp-reg* *fun-number-of-arguments-reg* *word-size* (* *array-header-size* *word-size*)))
    (inst :mov (@ *heap-header-reg*) *tmp-reg*)
    ;; increase count with  array header size and copy data
    (inst :add *fun-number-of-arguments-reg* *array-header-size*)
    (inst :label copy-loop-label)
    (inst :cmp *fun-number-of-arguments-reg* 0)
    (inst :jump-fixup :je copy-done-label)
    (inst :dec *fun-number-of-arguments-reg*)
    (inst :mov *tmp-reg* (@ array *fun-number-of-arguments-reg* 8 (- *pointer-tag*)))
    (inst :mov (@ res *fun-number-of-arguments-reg* 8) *tmp-reg*)
    (inst :jump-fixup :jmp copy-loop-label)
    (inst :label copy-done-label)
    (inst :add res *pointer-tag*)))


(define-vop %set-simple-array-tag (res :register) ((array :register))
  (inst :mov (@ array nil nil (- *pointer-tag*)) (get-extended-tag 'simple-array))
  (inst :mov res array))

(define-vop %set-string-tag (res :register) ((array :register))
  (inst :mov (@ array nil nil (- *pointer-tag*)) (get-extended-tag 'string))
  (inst :mov res array))
