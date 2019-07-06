(in-package :clcomp)

(declaim (optimize (speed 0) (safety 3) (debug 3)))


(defparameter *segment-instructions* nil)

(defun inst (&rest rest)
  (push rest *segment-instructions*))

(defparameter *known-vops* (make-hash-table))

(defstruct vop name arguments res fun)

(defun get-vop (name)
  (gethash name *known-vops*))

(defun get-args-count (vop)
  (length (vop-arguments vop)))

(defun get-args-types (vop)
  (mapcar 'second (vop-arguments vop)))

(defun get-res-type (vop)
  (second (vop-res vop)))

(defmacro define-vop (name res (&rest arguments) &body body)
  `(setf (gethash ',name *known-vops*)
	 (make-vop :name ',name
		   :res ',res
		   :arguments ',arguments
		   :fun (lambda ,(cons (first res) (mapcar 'car arguments))
			  (progn
			    ,@body)))))

(defun get-vop-code (vop  args)
  (let ((*segment-instructions* nil))
    (reverse
     (apply (vop-fun vop) args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vop-regs-listify (dest no-more-arguments-label)
  ;; dest should be list tagged pointer
  (dolist (arg-reg *fun-arguments-regs*)
    ;; set CAR
    (inst :mov (@ dest nil nil (- *list-tag*)) arg-reg)
    ;; no more args ?
    (inst :sub :RCX (fixnumize 1))
    (inst :cmp :RCX 0)
    (inst :jump-fixup :je no-more-arguments-label)
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
  (inst :sub :RCX (fixnumize 1))
  (inst :cmp :RCX 0)
  (inst :jump-fixup :je no-more-arguments-label)
  ;; move to next cons
  (inst :add dest (* 2 *word-size*))
  (inst :mov (@ dest nil nil (- (+ *list-tag* *word-size*))) dest)
  (inst :add temp-reg1 1)
  (inst :jump-fixup :jmp :start))

(defun vop-call-listify ()
  (inst :cmp :RCX 0)
  (inst :jump-fixup :je :zero-args) ; FIXME, je is for both short and near jumps, look for this in assembler
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
  (inst :jump-fixup :jmp :end)
  ;; FIXME, this is not ok
  (inst :label :set-cdr)
  (inst :mov (@ :R10 nil nil (- *word-size* *list-tag*)) *nil*)
  (inst :label :end))
