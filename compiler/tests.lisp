(in-package #:clcomp)

(defparameter *tests*
  (list
   ;; ADD
   (list (make-instruction :add :RAX :RCX) '(#x48 #x01 #xc8))
   (list (make-instruction :add :RCX :R10) '(#x4c #x01 #xd1))
   (list (make-instruction :add :r15 :RAX) '(#x49 #x01 #xC7))
   (list (make-instruction :add (@ :R10) :RCX) '(#x49 #x01 #x0a))
   (list (make-instruction :add (@ :R10 :R12 nil nil) :RAX) '(#x4b #x01 #x04 #x22))
   (list (make-instruction :add (@ :R8 nil nil -10) :R15) '(#x4d #x01 #x78 #xf6))
   (list (make-instruction :add (@ :RDX :R15 8 129) :RCX) '(#x4a #x01 #x8c #xfa #x81 #x00 #x00 #x00))
   (list (make-instruction :add :R14 -1) '(#x49 #x83 #xc6 #xff ))
   (list (make-instruction :add :RAX 10) '(#x48 #x83 #xc0 #x0a))
   (list (make-instruction :add :R15 -400) '(#x49 #x81 #xc7 #x70 #xfe #xff #xff))
   (list (make-instruction :add :eax (@ :ecx)) '(#x67 #x03 #x01))
   (list (make-instruction :add :rcx (@ 10000)) '(#x48 #x03 #x0C #x25 #x10 #x27 #x00 #x00))
   ;; MOV
   (list (make-instruction :mov :RAX :RCX) '(#x48 #x89 #xC8))
   (list (make-instruction :mov (@ nil :RAX 2 nil) :RCX) '(#x48 #x89 #x0C #x45 #x00 #x00 #x00 #x00))
   (list (make-instruction :mov (@ :RAX 2) :RCX) '(#x48 #x89 #x0C #x45 #x00 #x00 #x00 #x00))
   (list (make-instruction :mov (@ :R11) :RAX) '(#x49 #x89 #x03))
   (list (make-instruction :mov (@ :R11 nil nil nil) :RAX) '(#x49 #x89 #x03))
   (list (make-instruction :mov (@ :RAX :RBX 4 20) :RCX) '(#x48 #x89 #x4C #x98 #x14))
   (list (make-instruction :mov (@ :RAX :RBX nil 127) :RBX) '(#x48 #x89 #x5C #x18 #x7F))
   (list (make-instruction :mov (@ nil :RAX 8 nil) :RBX) '(#x48 #x89 #x1C #xC5 #x00 #x00 #x00 #x00))
   (list (make-instruction :mov :ECX (@ :EAX 2)) '(#x67 #x8B #x0C #x45 #x00 #x00 #x00 #x00))
   (list (make-instruction :mov :R15 35184372088832) '(#x49 #xbf #x00 #x00 #x00 #x00 #x00 #x20 #x00 #x00))
   (list (make-instruction :mov :R15 -35184372088832) '(#x49 #xbf #x00 #x00 #x00 #x00 #x00 #xe0 #xff #xff))
   (list (make-instruction :mov :R15 3) '(#x49 #xc7 #xc7 #x03 #x00 #x00 #x00))
   (list (make-instruction :mov :RAX -3) '(#x48 #xc7 #xc0 #xfd #xff #xff #xff))
   (list (make-instruction :mov :RCX (@ :RSP)) '(#x48 #x8b #x0c #x24))
   (list (make-instruction :mov :RAX (@ :RSP :RCX 8 20)) '(#x48 #x8b #x44 #xcc #x14))
   (list (make-instruction :mov :RAX (@ :RBP :RCX 8 20)) '(#x48 #x8b #x44 #xcd #x14 ))
   (list (make-instruction :mov :RBP (@ :RSP)) '(#x48 #x8b #x2c #x24))
   (list (make-instruction :MOV :RAX (@ nil :RBP 2 nil)) '(#x48 #x8b #x04 #x6d #x00 #x00 #x00 #x00))
   (list (make-instruction :MOV :RAX (@ :RBP :RCX nil nil)) '(#x48 #x8b #x44 #x0d #x00))
   (list (make-instruction :MOV :RAX (@ :RSP)) '(#x48 #x8b #x04 #x24))
   (list (make-instruction :MOV :RAX (@ :R12)) '(#x49 #x8b #x04 #x24))
   (list (make-instruction :MOV :RAX (@ :R12 nil nil -2)) '(#x49 #x8b #x44 #x24 #xfe))
   
   
   ;; LEA
   (list (make-instruction :lea :RAX (@ :RCX nil nil nil)) '(#x48 #x8D #x01))
   (list (make-instruction :lea :RBX (@ :RAX :RCX nil nil)) '(#x48 #x8D #x1C #x08))
   (list (make-instruction :lea :RBX (@ :RAX :RCX 8 127)) '(#x48 #x8D #x5C #xC8 #x7F))
   (list (make-instruction :lea :RBX (@ :RAX :RCX 8 128)) '(#x48 #x8D #x9C #xC8 #x80 #x00 #x00 #x00))
   ;; SUB
   (list (make-instruction :sub :RBX 33554432) '(#x48 #x81 #xEB #x00 #x00 #x00 #x02))
   (list (make-instruction :sub :RBX 1) '(#x48 #x83 #xEB #x01))
   (list (make-instruction :sub :RAX (@ :RBX)) '(#x48 #x2b #x03))
   (list (make-instruction :sub :RAX 10) '(#x48 #x83 #xe8 #x0a))
   (list (make-instruction :sub :R15 -23) '(#x49 #x83 #xef #xe9))
   (list (make-instruction :sub :R12 -12123) '(#x49 #x81 #xec #xa5 #xd0 #xff #xff))
   (list (make-instruction :sub :RCX (@ :R15 :RBX)) '(#x49 #x2b #x0c #X1f))
   (list (make-instruction :sub :RDX (@ :RAX :RCX 4 213)) '(#x48 #x2b #x94 #x88 #xd5 #x00 #x00 #x00))
   (list (make-instruction :sub :RBP 1122334455) '(#x48 #x81 #xed #xf7 #x76 #xe5 #x42))
   (list (make-instruction :sub (@ :RBP :RCX 4 123) :RAX) '(#x48 #x29 #x44 #x8d #x7b))

   ;; CMP
   

   ;; PUSH
   (list (make-instruction :push :RAX) '(#x50))
   (list (make-instruction :push :R8) '(#x41 #x50))
   (list (make-instruction :push :RDI) '(#x57))
   (list (make-instruction :push :R15) '(#x41 #x57))
   (list (make-instruction :push :RSP) '(#x54))
   (list (make-instruction :push :RBP) '(#x55))
   (list (make-instruction :push (@ :RAX)) '(#xff #x30))
   (list (make-instruction :push (@ :R8)) '(#x41 #xff #x30))
   (list (make-instruction :push (@ :R9)) '(#x41 #xff #x31))
   (list (make-instruction :push (@ :R12)) '(#x41 #xff #x34 #x24))
   (list (make-instruction :push (@ :R13)) '(#x41 #xff #x75 #x00))
   (list (make-instruction :push (@ :RSP)) '(#xff #x34 #x24))
   (list (make-instruction :push (@ :RAX :RBX 4 nil)) '(#xff #x34 #x98))
   (list (make-instruction :push (@ :RAX :RBX 4 -12)) '(#xff #x74 #x98 #xf4))
   (list (make-instruction :push (@ :RAX :R8)) '(#x42 #xff #x34 #x00))
   (list (make-instruction :push (@ :r8 :r12)) '(#x43 #xff #x34 #x20))
   (list (make-instruction :push (@ :R8 :R13)) '(#x43 #xff #x34 #x28))
   (list (make-instruction :push (@ :R8 :R13 8 -10)) '(#x43 #xff #x74 #xe8 #xf6))
   (list (make-instruction :push (@ :RSP :RBP nil -10)) '(#xff #x74 #x2c #xf6))
   
   ;; POP

   ;; CALL
   (list (make-instruction :call (@ :RAX)) '(#xff #x10))
   (list (make-instruction :call (@ :R11)) '(#x41 #xFF #x13 ))
   (list (make-instruction :call (@ :R12)) '(#x41 #xFF #x14 #x24))
   (list (make-instruction :call (@ :RAX :R12 4 10)) '(#x42 #xFF #x54 #xA0 #x0A))
   (list (make-instruction :call (@ :R13 nil nil -15)) '(#x41 #xFF #x55 #xF1))
   
   ;; MICS
   (list (make-instruction :ret) '(#xc3))
   ))

(defun run-instruction-tests ()
  (loop for (instruction expected) in *tests*
	collect (let ((res (assemble-instruction instruction)))
		  (if (equal res expected)
		      (list :OK instruction)
		      (list :ERROR instruction :res res :expected expected)))))
