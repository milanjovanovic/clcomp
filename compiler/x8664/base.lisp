(in-package :clcomp)

(define-vop %raw (res :register) ((arg :register))
  (inst :mov res arg)
  (inst :and res (ash -1 3)))

(define-vop eq (res :register :stack) ((arg1 :register :stack) (arg2 :register :stack))
  (let ((true-label (make-vop-label "true"))
	(exit-label (make-vop-label "exit")))
    ;; FIXME, we need way to automate stuff like this
    (inst :mov *tmp-reg* arg1)
    (inst :cmp *tmp-reg* arg2)
    (inst :jump-fixup :je true-label)
    (inst :mov res *nil*)
    (inst :jump-fixup :jmp exit-label)
    (inst :label true-label)
    (inst :mov res *t*)
    (inst :label exit-label)))


;; FIXME, we need to branch between normal fun and closure
(define-vop call (f :register) ()
  (inst :mov f f))

;; size = number of qwords
(define-vop allocate (res :register) ((size :immediate :register :stack))
  (inst :mov res (@ *heap-header-reg*))
  (inst :mov *tmp-reg* size)
  (inst :lea *tmp-reg* (@ res *tmp-reg* *word-size*))
  (inst :mov (@ *heap-header-reg*) *tmp-reg*))

;;; closures support
;;; closure format = | header qword | env ptr | fun ptr
(define-vop make-closure (res :register) ((env :register :stack)
					  (fun :register :stack))
  (inline-vop 'allocate res 3 $stack-top-operand$)
  (inst :mov (@ res) (get-extended-tag 'closure))
  (inst :mov *tmp-reg* env)
  (inst :mov (@ res nil nil *word-size*) *tmp-reg*)
  (inst :mov *tmp-reg* fun)
  (inst :mov (@ res nil nil (* 2 *word-size*)) *tmp-reg* )
  (inst :add res *pointer-tag*))

;; closure-env format =  | header qword | count (row 64 bit int) | stack-info-or-bcell-ptr | ...
(define-vop make-closure-env (res :register) ((count :immediate))
  (inline-vop 'allocate res (+ 2 count) $stack-top-operand$)
  (inst :mov (@ res) (get-extended-tag 'closure-env))
  (inst :mov (@ res nil nil *word-size*) count) ;; maybe put count in upper half of first qword
  (inst :add res *pointer-tag*))

(define-vop get-from-closure-env (res :register :stack) ((env :register :stack)
							 (index :immediate))
  (inst :mov *tmp-reg* env)
  ;; get bcell
  (inst :mov *tmp-reg* (@ *tmp-reg* nil nil (- (+ (* 1 *word-size*) ;; header type qword
						  (* index *word-size*))
					       *pointer-tag*)))
  ;; get value
  (inst :mov *tmp-reg* (@ *tmp-reg* nil nil (- (* 1 *word-size*) ;; header type qword
					       *pointer-tag*)))
  (inst :mov res *tmp-reg*))

(define-vop set-bcell-in-closure-env () ((env :register)
					 (index :immediate)
					 (bcell :register :stack))
  (inst :mov *tmp-reg* bcell)
  (inst :mov (@ env nil nil (- (+ (* 1 *word-size*)
				  (* index *word-size*))
			       *pointer-tag*))
	*tmp-reg*))

;; bcell format =  | header qword | qword |
(define-vop make-bcell (res :register) ((value :register))
  (inline-vop 'allocate res 2 $stack-top-operand$)
  (inst :mov (@ res) (get-extended-tag 'bcell))
  (inst :mov (@ res nil nil *word-size*) value)
  (inst :add res *pointer-tag*))

(define-vop get-bcell-value (res :register) ((bcell :register :stack))
  (inst :mov *tmp-reg* bcell)
  (inst :mov *tmp-reg* (@ *tmp-reg* nil nil (- *word-size* *pointer-tag*)))
  (inst :mov res *tmp-reg*))

(define-vop set-bcell-value (res :register :stack) ((bcell :register) (value :register :stack))
  (inst :mov *tmp-reg* value)
  (inst :mov (@ bcell nil nil (- *word-size* *pointer-tag*)) *tmp-reg*)
  (inst :mov res *tmp-reg*))

(define-vop bla (res :register) ((arg :register))
  (inst :ud2))
