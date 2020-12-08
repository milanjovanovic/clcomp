(in-package #:clcomp)

;;; instruction templates

;;;; NOTE
;;;; it's *CRUCIA(L in assembler to match operands with documentation
;;;; if arguments are MEM/REG in documentation arguments are in different order in ModRM byte
;;; (:reg64 (:reg64 :addr)) -> example, if we have (:reg64 :reg64) assembler will produce different bytecode
;;; 
;;; ADD
(define-inst-template :add ((:reg64 :addr) :reg64) ()
		      nil nil #x01 nil)

(define-inst-template :add (:reg32 :reg32) ()
		      nil nil #x01 nil)

(define-inst-template :add (:addr :reg32) ()
		      #x67 nil #x01 nil)

(define-inst-template :add (:reg64 :addr) ()
		      nil nil #x03 nil)
(define-inst-template :add (:reg32 :addr32) ()
		      #x67 nil #x03 nil)

(define-inst-template :add (:reg64 :imm32) ()
		      nil nil #x81 nil)

(define-inst-template :add (:reg32 :imm32) ()
		      nil nil #x81 nil)

(define-inst-template :add (:reg64 :imm8) ()
		      nil nil #x83 nil)

;;; SUB
(define-inst-template :sub (:reg64 :imm32) ()
		      nil nil #x81 #x28)

(define-inst-template :sub (:reg64 :imm8) ()
		      nil nil #x83 #x28)

(define-inst-template :sub (:reg64 :addr) ()
		      nil nil #x2b nil)

(define-inst-template :sub ((:reg64 :addr) :reg64) ()
		      nil nil #x29 nil)

;;; DEC/INC
(define-inst-template :dec (:reg64) ()
		      nil nil #xff #x8)

(define-inst-template :dec (:reg32) ()
		      nil nil #xff #x8)

(define-inst-template :inc (:reg64) ()
		      nil nil #xff #x0)

(define-inst-template :inc (:reg32) ()
		      nil nil #xff #x0)

(define-inst-template :neg (:reg64) ()
		      nil nil #xF7 #x18)

;;; IMUL
(define-inst-template :imul (:reg64 (:reg64 :addr)) ()
		      nil #x0F #xAF nil)

;;; LOGICAL
(define-inst-template :xor ((:reg64 :addr) :reg64) ()
		      nil nil #x31 nil)

(define-inst-template :xor ((:reg64 :addr) :imm32) ()
		      nil nil #x81 #x30)


(define-inst-template :or ((:reg64 :addr) :imm32) ()
		      nil nil #x81 #x08)

(define-inst-template :or ((:reg64 :addr) :imm32) ()
		      nil nil #x81 #x30)


(define-inst-template :and ((:reg64 :addr) :imm32) ()
		      nil nil #x81 #x20)

(define-inst-template :and ((:reg64 :addr) :reg64) ()
		      nil nil #x21 nil)

;;; MOV
(define-inst-template :mov (:reg64 :reg64) ()
		      nil nil #x89 nil)

(define-inst-template :mov (:reg32 :reg32) ()
		      nil nil #x89 nil)

(define-inst-template :mov (:reg16 :reg16) ()
		      #x66 nil #x89 nil)

(define-inst-template :mov (:reg8 :reg8) ()
		      nil nil #x88 nil)


(define-inst-template :mov (:addr :reg64) ()
		      nil nil #x89 nil)
(define-inst-template :mov (:addr32 :reg32) ()
		      #x67 nil #x89 nil)

(define-inst-template :mov (:reg64 :addr) ()
		      nil nil #x8b nil)
(define-inst-template :mov (:reg32 :addr32) ()
		      #x67 nil #x8b nil)

(define-inst-template :mov (:reg64 :imm64) (+r)
		      nil nil #xb8 nil)

(define-inst-template :mov (:reg64 :imm32) ()
		      nil nil #xc7 #x00)

(define-inst-template :mov ((:addr64 :addr32) :imm32) ()
		      nil nil #xc7 #x00)



(define-inst-template :cmovs (:reg64 (:reg64 :addr64)) ()
		      nil #x0f #x48 nil)

(define-inst-template :cmovle (:reg64 (:reg64 :addr64)) ()
		      nil #x0f #x4E nil)

(define-inst-template :cmovnle (:reg64 (:reg64 :addr64)) ()
		      nil #x0f #x4F nil)

;;; LEA
(define-inst-template :lea (:reg64 :addr) ()
		      nil nil #x8D nil)


;;; CMP

(define-inst-template :cmp (:reg64 :reg64) ()
		      nil nil #x39 nil)

(define-inst-template :cmp (:reg64 :addr) ()
		      nil nil #x3b nil)

(define-inst-template :cmp (:addr :reg64) ()
		      nil nil #x39 nil)

(define-inst-template :cmp (:reg8 :imm8) ()
		      nil nil #x80 #x38)

(define-inst-template :cmp (:reg64 :imm32) ()
		      nil nil #x81 #x38)

(define-inst-template :cmp (:reg64 :imm8) ()
		      nil nil #x83 #x38)

(define-inst-template :cmp (:addr8 :uimm8) ()
		      nil nil #x80 #x38)

;;; TEST
(define-inst-template :test (:reg64 :reg64) ()
		      nil nil #x85 nil)

(define-inst-template :test (:reg64 :addr) ()
		      nil nil #x85 nil)

(define-inst-template :test (:addr :reg64) ()
		      nil nil #x85 nil)

(define-inst-template :test (:reg8 :imm8) ()
		      nil nil #xF6 #x00)

(define-inst-template :test (:reg64 :imm32) ()
		      nil nil #xF7 #x00)


;;; PUSH
(define-inst-template :push (:reg64) (+r +v64) 
		      nil nil #x50 nil)

(define-inst-template :push (:addr) (+v64)
		      nil nil #xFF #x30) 

#+nil
(define-inst-template :push (:imm64) ()
		      nil nil nil #x68 nil)


;;; POP
(define-inst-template :pop (:reg64) (+r +v64)
		      nil nil #x58 nil)

(define-inst-template :pop (:addr) ()
		      nil nil #x8F #x00)

(define-inst-template :neg (:reg64) ()
		      nil nil #xF7 #xD8)

;;; CALL
(define-inst-template :call ((:reg64 :addr64)) (+v64)
		      nil nil #xff #x10)

;; JMP
#+nil
(define-inst-template :jmp (:imm8) ()
		      nil nil #xeb nil)

(define-inst-template :jmp (:imm32) ()
		      nil nil #xe9 nil)

;;; fixnme, look at assembling and jmm* instruction size
#+nil
(define-inst-template :jmp (:imm8) ()
		      nil nil #xeb nil)

;;; FIXME, we are calculating assembling with every-jump-instruction=5 bytes
;;; maybe instructions below will brake that ?!?!
(define-inst-template :jl (:imm32) ()
		      #x0f nil #x8C nil)

(define-inst-template :jle (:imm32) ()
		      #x0f nil #x8E nil)

(define-inst-template :jnge (:imm32) ()
		      #x0f nil #x8C nil)

(define-inst-template :jg (:imm32) ()
		      #x0f nil #x8f nil)

(define-inst-template :jnle (:imm32) ()
		      #x0f nil #x8f nil)

;; JE
(define-inst-template :je (:imm32) ()
		      #x0f nil #x84 nil)

;; JNE
(define-inst-template :jne (:imm32) ()
		      #x0f nil #x85 nil)

(define-inst-template :jnbe (:imm32) ()
		      #x0f nil #x87 nil)

(define-inst-template :jge (:imm32) ()
		      #x0f nil #x8D nil)

(define-inst-template :jnl (:imm32) ()
		      #x0f nil #x8D nil)

;; (define-inst-template :jrcxz (:imm8) ()
;; 		      nil nil #xE3 nil)

;; (define-inst-template :jns (:imm8) ()
;; 		      nil nil #x79 nil)

(define-inst-template :jnb (:imm32) ()
		      #x0f nil #x83 nil)

(define-inst-template :js (:imm32) ()
		      #x0f nil #x88 nil)

;; shift

(define-inst-template :shl (:reg64 :imm8) ()
		      nil nil #xC1 #x20)

(define-inst-template :shr (:reg64 :imm8) ()
		      nil nil #xC1 #x28)

(define-inst-template :sar (:reg64 :imm8) ()
		      nil nil #xC1 #x38)

(define-inst-template :shl (:reg64 :cl) ()
		      nil nil #xD3 #x20)

(define-inst-template :shr (:reg64 :cl) ()
		      nil nil #xD3 #x28)

(define-inst-template :sar (:reg64 :cl) ()
		      nil nil #xD3 #x38)


(define-inst-template :ret () ()
		      nil nil #xc3 nil)

(define-inst-template :clc () ()
		      nil nil #xf8 nil)

(define-inst-template :ud2 () ()
		      nil nil '(#x0f #x0b) nil)


;;; SSE1
(define-inst-template :cvtsi2ss (:xmmreg :reg64) ()
		      #xf3 #x0f #x2a nil)

(define-inst-template :movups (:xmmreg (:xmmreg :xmmaddr)) ()
		      nil #x0f #x10 nil)
