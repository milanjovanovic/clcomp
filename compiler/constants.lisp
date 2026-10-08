(in-package #:clcomp)

(defparameter *base-pointer-reg* :RBP)
(defparameter *stack-pointer-reg* :RSP)
(defparameter *instruction-pointer-reg* :RIP)
(defparameter *fun-address-reg* :RAX)
(defparameter *fun-number-of-arguments-reg* :RCX)
(defparameter *fun-number-of-ret-values-reg* :RCX)
(defparameter *fun-arguments-regs* '(:RDX :RDI :R8 :R9))

(defparameter *scratch-regs* '(:R10 :R11))
(defparameter *tmp-reg* :R10)

;;; this one is tricky, it's used in define-vop as default tmp register in a case of aliasinge
;;; so it can't be used as temporary in VOP body
(defparameter *tmp-reg-2* :R11)
;;; NOTE. keep *preserved-regs* always at even number, stack alignment
(defparameter *preserved-regs* '(:R12 :R13 :R14 :RBX :RSI))
(defparameter *heap-header-reg* :R15)

(defparameter *allocation-size* 16)
(defparameter *word-size* 8)

(defparameter *fixnum-tag-size* 1)
(defparameter *fixnum-mask* 1)

(defparameter *tag-size* 4)
(defparameter *mask* 15)

;; NOTE, don't change *fixnum-tag
;; in some assembly we are skipping lea DEST, [SRC-*fixnum-tag*] because we know it is 0
(defparameter *fixnum-tag* 0)
(defparameter *pointer-tag* 1)
(defparameter *list-tag* 3)
(defparameter *function-tag* 5)
(defparameter *char-tag* 7)
(defparameter *symbol-tag* 9)
(defparameter *single-float-tag* 11)
(defparameter *widetag-tag* 15)

(defparameter *exteneded-tag-size* 8)
(defparameter *extended-tag-mask* 255)

;;; first qword in heap allocated objects is type
;;; we use the same 4 bit tagging scheme like in the immediate objects case
;;; fixnum, char, single-float, all the others are free
;;; we use *widetag-tag* as tag for other-heap-allocated-types
;;; every type in *extended-tags* need to have #b1111 as low 4 bits

(defparameter *other-boxed-type* *widetag-tag*)

(defparameter *extended-tags*
  '((struct 15)
    (simple-array 31)
    (string 47)
    (closure 63)
    (closure-env 79)
    ;; closure related, variable binding cell
    (bcell 95)
    ))

(defun get-extended-tag (what)
  (second (assoc what *extended-tags*)))

;;; tag 1 is free ??

(defparameter *nil* 536870914)
(defparameter *t* 536870927)

(defparameter *most-positive-fixnum* (- (expt 2 (- (* *word-size* 8)
						   (+ 1 *fixnum-tag-size*))) 1))

(defparameter *most-negative-fixnum* (- (expt 2 (- (* *word-size* 8)
						   (+ 1 *fixnum-tag-size*)))))


(defparameter *array-header-size* 4) ;; look at lispo.h
(defparameter *struct-header-size* 3)

(defun fixnumize (num)
  (if (and (> num *most-negative-fixnum*)
	   (< num *most-positive-fixnum* ))
      (ash num *fixnum-tag-size*)
      (error "Number is to big to be fixnum !!!")))

(defun characterize (char)
  (let ((code (char-code char)))
    (+ (ash code *tag-size*) *char-tag*)))
