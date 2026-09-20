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
(defparameter *preserved-regs* '(:R12 :R13 :R14 :RBX :RSI :RSI))
(defparameter *heap-header-reg* :R15)

(defparameter *closure-env-reg* :RBX)


(defparameter *allocation-size* 8)
(defparameter *word-size* 8)

(defparameter *tag-size* 3)
(defparameter *mask* 7)

(defparameter *fixnum-tag* 0)
(defparameter *pointer-tag* 1)
(defparameter *list-tag* 2)
(defparameter *function-tag* 3)
(defparameter *char-tag* 4)
(defparameter *symbol-tag* 5)
(defparameter *single-float-tag* 6)

(defparameter *exteneded-tag-size* 8)
(defparameter *extended-tag-mask* 255)

;;; first qword in heap allocated objects is type
;;; we use the same 3 bit tagging scheme like in the immediate objects case
;;; fixnum, char, single-float, all the others are free
;;; we use *pointer-tag* as tag for other-heap-allocated-types
;;; every tupe in *extended-tags* need to have #b001 as low 3 bits

(defparameter *other-boxed-type* *pointer-tag*)

(defparameter *extended-tags*
  '((struct 193)
    (simple-array 209)
    (string 217)))

(defparameter *largest-extended-tag* 249)

(defun get-extended-tag (what)
  (second (assoc what *extended-tags*)))

;;; tag 1 is free ??

(defparameter *nil* 536870914)
(defparameter *t* 536870927)

(defparameter *most-positive-fixnum* (- (expt 2 (- (* *word-size* 8)
						   (+ 1 *tag-size*))) 1))

(defparameter *most-negative-fixnum* (- (expt 2 (- (* *word-size* 8)
						   (+ 1 *tag-size*)))))


(defparameter *array-header-size* 4) ;; look at lispo.h
(defparameter *struct-header-size* 3)

(defun fixnumize (num)
  (if (and (> num *most-negative-fixnum*)
	   (< num *most-positive-fixnum* ))
      (ash num *tag-size*)
      (error "Number is to big to be fixnum !!!")))

(defun characterize (char)
  (let ((code (char-code char)))
    (+ (ash code *tag-size*) *char-tag*)))


;;; how to encode heap allocated objects
;;; use the same tagging scheme for heap allocated objets
;;; so for single-floats, chars, fixnums it's just one qword-heap-allocated objet
;;; copy to heap is just one mov [ptr], value
;;; if heap allocated object first qword has *pointer-tag* then we look at header qword as only type
;;; this complex types need to have 3 lower bits as *pointer-tag*
