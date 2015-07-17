;;; instruction templates

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

(define-inst-template :mov (:reg64 :imm64) (/d)
		      nil #x48 #xb8 nil)
#+nil(define-inst-template :mov (:reg32 :imm32) ()
		      nil nil nil nil)


(define-inst-template :push (:reg64) (/d) 
		      nil nil #x50 nil)

(define-inst-template :push (:imm64) ()
		      nil #x48 #x68 nil)

(define-inst-template :push (:addr) ()
		      nil nil #xff #b00110000)

(define-inst-template :pop (:reg64) (/d)
		      nil nil #x58 nil)

(define-inst-template :lea (:reg64 :addr) ()
		      nil #x48 #x8D nil)

(define-inst-template :add (:reg64 :reg64) ()
		      nil #x48 #x01 nil)



;;; instrucions

#+nil
(
;;; instruction examples
 ;;
 (inst :mov :RAX 10)
 (inst :mov :RAX :RBX)
 (inst :leal :RBX (@ :RCX :RAX 4 16))
 (inst :push RBX)
 (inst :push (@ :RAX))
 )
