(in-package :clcomp)

(defun inst (&rest rest)
  (push rest *segment-instructions*))

(defun assembly-instructions (instructions)
  (let (code)
    (dolist (inst (reverse instructions))
      (setf code (append code (encode-instruction (first inst) (reparse-operands (rest inst))))))
    code))

(defun make-fixnum (num)
  (ash num 3))

(defun vop-regs-listify (dest no-more-arguments-label)
  ;; dest should be list tagged pointer
  (dolist (arg-reg *fun-arguments-regs*)
    ;; set CAR
    (inst :mov (@ dest nil nil (- *list-tag*)) arg-reg)
    ;;; no more args ?
    (inst :sub :RCX (make-fixnum 1))
    (inst :cmp :RCX 0)
    (inst :je no-more-arguments-label)
    ;; set CDR
    (inst :add dest (* 2 *word-size*))
    (inst :mov (@ dest nil nil (- (+ *list-tag* *word-size*))) dest))) ; FIXME, we need second register here

(defun vop-stack-listify (dest temp-reg1 temp-reg2 no-more-arguments-label)
  (inst :mov temp-reg1 1)
  (inst :label :start)
  ;; FIXME, check args location on stack, we still didn't decided where arguments are on stack
  (inst :mov temp-reg2 (@ *base-pointer-reg* temp-reg1 *word-size* nil))
  (inst :mov (@ dest nil nil (- *list-tag*)) temp-reg2)
  ;; no more args ?
  (inst :sub :RCX (make-fixnum 1))
  (inst :cmp :RCX 0)
  (inst :je no-more-arguments-label)
  ;; move to next cons
  (inst :add dest (* 2 *word-size*))
  (inst :mov (@ dest nil nil (- (+ *list-tag* *word-size*))) dest)
  (inst :add temp-reg1 1)
  (inst :jmp :start))

(defun vop-call-listify ()
  (inst :cmp :RCX 0)
  (inst :je :zero-args) ; FIXME, je is for both short and near jumps, look for this in assembler
  ;; start allocation
  ;; FIXME, is allocation ok ? :RCX is fixnum (shifted 3 times) so 1 is already one word
  ;; this will not work if we change number of tag bits
  (inst :mov :R9 (@ *heap-header-reg*))
  (inst :mov *return-value-reg* :R9)
  (inst :lea :R9 (@ :R9 :RCX nil nil))
  (inst :mov (@ *heap-header-reg*) :R9)
  (inst :mov :R9 *return-value-reg*)
  (inst :add *return-value-reg* *list-tag*)
  (inst :mov :R9 *return-value-reg*)
  (vop-regs-listify :R9 :set-cdr)
  (vop-stack-listify :R9 :R10 :R11 :set-cdr)
  (inst :label :zero-args)
  (inst :mov *return-value-reg* *nil*)
  (inst :jmp :end)
  ;; FIXME, this is not ok
  (inst :label :set-cdr)
  (inst :mov (@ :R10 nil nil (- *word-size* *list-tag*)) *nil*)
  (inst :label :end))

(defun vop-cons (arg1 arg2 res)
  (gen-instructions
   ;; fixme, test heap end and since we dont' have GC yet just die
   (inst :mov res (@ *heap-header-reg*))
   (inst :lea res (@ res nil nil (* 2 *word-size*)))
   (inst :mov res (@ *heap-header-reg*))
   (inst :lea res (@ res nil nil (- (* 2 *word-size*))))
   (inst :mov (@ res) arg1)
   (inst :mov (@ res nil nil 8) arg2)
   (inst :add res *list-tag*)))

(defun vop-car (arg1 res)
  (gen-instructions
   (inst :lea res (@ arg1 nil nil (- *list-tag*)))
   (inst :test arg1 *mask*)
   (inst :jnbe :not-cons)
   (inst :mov arg1 (@ arg1))
   :not-cons
   (inst :mov res *nil*)))


(defun vop-cdr (arg1)
  (declare (ignore arg1)))




