(in-package #:clcomp)

;;; instruction templates

;;; ADD
(define-inst-template :add ((:reg64 :addr) :reg64) ()
		      nil #x48 #x01 nil)

(define-inst-template :add (:reg32 :reg32) ()
		      nil nil #x01 nil)

(define-inst-template :add (:addr :reg32) ()
		      #x67 nil #x01 nil)

(define-inst-template :add (:reg64 :addr) ()
		      nil #x48 #x03 nil)
(define-inst-template :add (:reg32 :addr) ()
		      #x67 nil #x03 nil)

(define-inst-template :add (:reg64 :imm32) ()
		      nil #x48 #x81 nil)

(define-inst-template :add (:reg32 :imm32) ()
		      nil nil #x81 nil)

(define-inst-template :add (:reg64 :imm8) ()
		      nil #x48 #x83 nil)

;;; SUB
(define-inst-template :sub (:reg64 :imm32) ()
		      nil #x48 #x81 #x28)

(define-inst-template :sub (:reg64 :imm8) ()
		      nil #x48 #x83 #x28)

(define-inst-template :sub (:reg64 :addr) ()
		      nil #x48 #x2b nil)

(define-inst-template :sub ((:reg64 :addr) :reg64) ()
		      nil #x48 #x29 nil)


;;; MOV
(define-inst-template :mov (:reg64 :reg64) ()
		      nil #x48 #x89 nil)

(define-inst-template :mov (:reg32 :reg32) ()
		      nil nil #x89 nil)

(define-inst-template :mov (:reg16 :reg16) ()
		      #x66 nil #x89 nil)

(define-inst-template :mov (:reg8 :reg8) ()
		      nil nil #x88 nil)


(define-inst-template :mov (:addr :reg64) ()
		      nil #x48 #x89 nil)
(define-inst-template :mov (:addr :reg32) ()
		      #x67 nil #x89 nil)

(define-inst-template :mov (:reg64 :addr) ()
		      nil #x48 #x8b nil)
(define-inst-template :mov (:reg32 :addr) ()
		      #x67 nil #x8b nil)

(define-inst-template :mov (:reg64 :imm64) (+r)
		      nil #x48 #xb8 nil)

(define-inst-template :mov (:reg64 :imm32) ()
		      nil #x48 #xc7 #x00)

(define-inst-template :mov (:addr :imm32) ()
		      nil #x48 #xc7 #x00)


;;; LEA
(define-inst-template :lea (:reg64 :addr) ()
		      nil #x48 #x8D nil)


;;; CMP

(define-inst-template :cmp (:reg64 :reg64) ()
		      nil #x48 #x39 nil)

(define-inst-template :cmp (:reg64 :addr) ()
		      nil #x48 #x3b nil)

(define-inst-template :cmp (:addr :reg64) ()
		      nil #x48 #x39 nil)

(define-inst-template :cmp (:reg8 :imm8) ()
		      nil nil #x80 #x38)

(define-inst-template :cmp (:reg64 :imm32) ()
		      nil #x48 #x81 #x38)

(define-inst-template :cmp (:reg64 :imm8) ()
		      nil #x48 #x83 #x38)

;;; TEST
(define-inst-template :test (:reg64 :reg64) ()
		      nil #x48 #x85 nil)

(define-inst-template :test (:reg64 :addr) ()
		      nil #x48 #x85 nil)

(define-inst-template :test (:addr :reg64) ()
		      nil #x48 #x85 nil)

(define-inst-template :test (:reg8 :imm8) ()
		      nil nil #xF6 #x00)

(define-inst-template :test (:reg64 :imm32) ()
		      nil #x48 #xF7 #x00)


;;; PUSH
(define-inst-template :push (:reg64) (+r) 
		      nil nil #x50 nil)

(define-inst-template :push (:addr) ()
		      nil nil #xFF #x30)

#+nil
(define-inst-template :push (:imm64) ()
		      nil #x48 #x68 nil)


;;; POP
(define-inst-template :pop (:reg64) (+r)
		      nil nil #x58 nil)

(define-inst-template :pop (:addr) ()
		      nil nil #x8F #x00)

;;; CALL
(define-inst-template :call ((:reg64 :addr)) ()
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


(define-inst-template :jl (:imm32) ()
		      #x0f nil #x8C nil)

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




(define-inst-template :ret () ()
		      nil nil #xc3 nil)

(define-inst-template :clc () ()
		      nil nil #xf8 nil)
