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

(define-inst-template :sub (:addr :reg64) ()
		      nil #x48 #x29 nil)


;;; MOV
(define-inst-template :mov (:reg64 :reg64) ()
		      nil #x48 #x89 nil)
(define-inst-template :mov (:reg32 :reg32) ()
		      nil nil #x89 nil)x
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
		      nil #x48 #xc7 nil)

#+nil
(define-inst-template :mov (:reg32 :imm32) (+r)
		      nil nil #xb8 nil)


;;; LEA
(define-inst-template :lea (:reg64 :addr) ()
		      nil #x48 #x8D nil)


;;; CMP


;;; PUSH
(define-inst-template :push (:reg64) (/d) 
		      nil nil #x50 nil)

(define-inst-template :push (:imm64) ()
		      nil #x48 #x68 nil)

(define-inst-template :push (:addr) ()
		      nil nil #xff #b00110000)


;;; POP
(define-inst-template :pop (:reg64) (/d)
		      nil nil #x58 nil)

;;; cmp
;;; jnbe
;;; jmp
;;; test
