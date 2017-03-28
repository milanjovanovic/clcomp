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

(defun regs-listify (dest no-more-arguments-label)
  ;; dest should be list tagged pointer
  (dolist (arg-reg *fun-arguments-regs*)
    ;; set CAR
    (inst :mov (@ dest nil nil (- *list-tag*)) arg-reg)
    (inst :sub :RCX (make-fixnum 1))
    (inst :cmp :RCX 0)
    (inst :je no-more-arguments-label)
    ;; set CDR
    (inst :add :R10 (* 2 *word-size*))
    (inst :mov (@ :R10 nil nil (- (+ *list-tag* *word-size*))) :R10)))

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
  (regs-listify :R9 :set-cdr)
  (inst :label :zero-args)
  (inst :mov *return-value-reg* *nil*)
  (inst :jmp :end)
  (inst :label :set-cdr)
  (inst :mov (@ :R10 nil nil (- (+ *list-tag* *word-size*))) :R10)
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



