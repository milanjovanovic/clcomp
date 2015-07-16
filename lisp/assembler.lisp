;;(declaim (optimize (speed 0) (debug 3)))

(defparameter *registers* (make-hash-table))

(defun defregister (register register-bits extend-bit)
  (setf (gethash register *registers*) (list register-bits extend-bit)))

(defun get-register (register)
  (gethash register *registers*))

(defun get-register-bits (register)
  (first (get-register register)))

(defun get-register-extend-bit (register)
  (second (get-register register)))

(defun is-register (what)
  (nth-value 1 (get-register what)))

(defun extended-register? (register)
  (let ((extend-bit (get-register-extend-bit register)))
   (and extend-bit (= 1 extend-bit))))

(defregister :rax #b000 #b0)
(defregister :rcx #b001 #b0)
(defregister :rdx #b010 #b0)
(defregister :rbx #b011 #b0)
(defregister :rsp #b100 #b0)
(defregister :rbp #b101 #b0)
(defregister :rsi #b110 #b0)
(defregister :rdi #b111 #b0)
(defregister :r8 #b000 #b1)
(defregister :r9 #b001 #b1)
(defregister :r10 #b010 #b1)
(defregister :r11 #b011 #b1)
(defregister :r12 #b100 #b1)
(defregister :r13 #b101 #b1)
(defregister :r14 #b110 #b1)
(defregister :r15 #b110 #b1)

(defparameter *instruction-pointer-register* :rip)
(defun ip-register? (register)
  (eq register *instruction-pointer-register*))

(defun rex (w r x b)
  (let ((rex 0))
    (setf (ldb (byte 4 4) rex) #b0100)	; fixed value
    (setf (ldb (byte 1 3) rex) w) ; #b1 extends operand to 64bit, if #b0 then default operand size is used
    (setf (ldb (byte 1 2) rex) r) ; bit extends MODRM.reg
    (setf (ldb (byte 1 1) rex) x) ; bit extends SIB.index
    (setf (ldb (byte 1 0) rex) b) ; bit extends MODRM.rm or SIB.base
    rex))

(defparameter *rex* #b01000000)
(defparameter *64bit-rex* #b01001000)

(defparameter *rex.b.position* 0)
(defparameter *rex.x.position* 1)
(defparameter *rex.r.position* 2)
(defparameter *rex.w.position* 3)
(defparameter *rex.extension.bits* 1)

#+nil(defun get-rex-extension-bit-for-sib (sib-position)
  (cond ((= sib-position *sib.base.position*) *rex.b.position*)
	((= sib-position *sib.index.position*) *rex.x.position*)))

(defun get-rex-extension-bit-for-modrm (modrm-position)
  (cond ((= modrm-position *modrm.rm.position*) *rex.b.position*)
	((= modrm-position *modrm.reg.position*) *rex.r.position*)
	(t (error "Unknown position"))))


(defun modrm (mod reg rm)
  (let ((modrm 0))
    (setf (ldb (byte 2 6) modrm) mod)
    (setf (ldb (byte 3 3) modrm) reg) ;; default destination operand
    (setf (ldb (byte 3 0) modrm) rm) ;; default source operand
    modrm))

(defparameter *modrm.mod.position* 6)
(defparameter *modrm.mod.bits* 2)
(defparameter *modrm.reg.position* 3)
(defparameter *modrm.rm.position* 0)
(defparameter *modrm.reg.rm.bits* 3)

(defparameter *modrm-register-addressing-mode* #b11)
(defparameter *modrm-4byte-displacement* #b10)
(defparameter *modrm-1byte-displacement* #b01)
(defparameter *modrm-register-indirect-or-sib* #b00)


(defun get-scale-bits (scale)
  (if (null scale)
      #b00
      (cond ((= scale 1) #b00)
	    ((= scale 2) #b01)
	    ((= scale 4) #b10)
	    ((= scale 8) #b11))))

(defun sib (base index scale)
  ;; SIB is one byte
  ;; | scale 2 bits | index 3 bits | base 3 bits|
  (let ((sib 0))
    (setf (ldb (byte 2 6) sib) scale)
    (setf (ldb (byte 3 3) sib) index)
    (setf (ldb (byte 3 0) sib) base)
    sib))

(defparameter *sib.scale.position* 6)
(defparameter *sib.scale.bits* 2)
(defparameter *sib.index.position* 3)
(defparameter *sib.index.bits* 3)
(defparameter *sib.base.position* 0)
(defparameter *sib.base.bits* 3)


(defparameter *instructions* (make-hash-table))

(defun get-mnemonic-templates (mnemonic)
  (gethash mnemonic *instructions*))

(defun add-inst-template (mnemonic operands flags prefixes rex opcode modrm)
  (let ((insts (gethash mnemonic *instructions*)))
    (setf (gethash mnemonic *instructions*)
	  (cons (list mnemonic operands flags prefixes rex opcode modrm)
		insts))))

(defmacro define-inst-template (mnemonic
				(&rest operands)
				flags
				prefixes
				rex
				opcode
				modrm)
  `(add-inst-template ,mnemonic ',operands ',flags ,prefixes ,rex ,opcode ,modrm))

(defun inst-template-mnemonic (inst-template)
  (nth 0 inst-template))
(defun inst-template-operands (inst-template)
  (nth 1 inst-template))
(defun inst-template-flags (inst-template)
  (nth 2 inst-template))
(defun inst-template-prefixes (inst-template)
  (nth 3 inst-template))
(defun inst-template-rex (inst-template)
  (nth 4 inst-template))
(defun inst-template-opcode (inst-template)
  (nth 5 inst-template))
(defun inst-template-modrm (inst-template)
  (nth 6 inst-template))

(defparameter *known-flags* '(register-added-to-opcode immediate-encoding))

(defun contain-flag (flag flags)
  (find flag flags))

(defun match-instruction-operand-type (inst-template-operand operand)
  (cond ((eq inst-template-operand :imm) (typep operand 'number))
	((eq inst-template-operand :reg) (is-register operand))
	((eq inst-template-operand :addr) (and (typep operand 'list)
					       (= (length operand) 4)))
	(t (error (format nil "Unknown instruction with argument ~A" operand)))))

(defun match-operands-type (inst-template-operands operands)
  (and (match-instruction-operand-type (first inst-template-operands) (first operands))
       (or (and (null (second inst-template-operands))
		(null (second operands)))
	   (match-instruction-operand-type (second inst-template-operands) (second operands)))))

(defun match-mnemonic-operands (mnemonic-templates operands)
  (let ((inst-number-of-operands (length operands)))
    (dolist (template mnemonic-templates)
      (let ((template-number-of-operands (length (inst-template-operands template))))
	(when (and (= inst-number-of-operands template-number-of-operands)
		   (match-operands-type (inst-template-operands template) operands))
	  (return-from  match-mnemonic-operands template))))))

(defun find-instruction-template (mnemonic operands)
  (let ((mnemonic-templates (get-mnemonic-templates mnemonic)))
    (unless mnemonic-templates
      (error (format nil "Unknown instruction ~A" mnemonic)))
    (or (match-mnemonic-operands mnemonic-templates operands)
	(error (format nil "Unknown instruction ~A with arguments ~A" mnemonic operands)))))

(defun encode-operands (rex modrm dest-operand source-operand template-dest-operand template-source-operand opcode-d-bit)
  (let (
	;; on opcode position 1 bit depends destination/source position in modrm byte
	(modrm-source-position (if opcode-d-bit *modrm.rm.position* *modrm.reg.position*))
	(modrm-extend-source-rex-position (if opcode-d-bit *rex.b.position* *rex.r.position*))
	(modrm-destination-position (if opcode-d-bit *modrm.reg.position* *modrm.rm.position*))
	(modrm-extend-dest-rex-position (if opcode-d-bit *rex.r.position* *rex.b.position*)))
    (cond ((eq template-source-operand :reg)
	   (setf (ldb (byte *modrm.reg.rm.bits* modrm-source-position) modrm) (get-register-bits source-operand))
	   (when (extended-register? source-operand)
	     (setf (ldb (byte *rex.extension.bits* modrm-extend-source-rex-position) rex) #b1))))
    (cond ((eq template-dest-operand :reg)
	   (setf (ldb (byte *modrm.reg.rm.bits* modrm-destination-position) modrm) (get-register-bits dest-operand))
	   (when (extended-register? dest-operand)
	     (setf (ldb (byte *rex.extension.bits* modrm-extend-dest-rex-position) rex) #b1))))
    (let ((effective-addr-operand (or (effective-address? source-operand) (effective-address? dest-operand)))
	  (modrm-operand-position (or (and (effective-address? source-operand) modrm-source-position)
				      (and (effective-address? dest-operand) modrm-destination-position))))
      (if (null effective-addr-operand)
	  (progn
	    (setf (ldb (byte *modrm.mod.bits* *modrm.mod.position*) modrm) #b11)
	    (list rex modrm nil nil))
	  ;; if there is memory operand
	  (destructuring-bind (rex modrm sib displacement) (encode-effective-memory-address rex modrm effective-addr-operand modrm-operand-position)
	    (list rex modrm sib displacement))))))

(defun encode-effective-memory-address (rex modrm effective-addr-operand modrm-operand-position)
  (let ((sib nil))
    (destructuring-bind (base index scale displacement) effective-addr-operand
      (cond ((and base (ip-register? base))
	     ;; if base register is instruction pointer we don't need SIB encoding
	     ;; can't have index or scale when base is instruction pointer register
	     (setf (ldb (byte *modrm.mod.bits* *modrm.mod.position*) modrm) #b00)
	     (setf (ldb (byte *modrm.reg.rm.bits* *modrm.rm.position*) modrm) #b101)
	     (list rex modrm sib (dword-as-byte-list (or displacement 0))))
	    ((null displacement)
	     (setf (ldb (byte *modrm.mod.bits* *modrm.mod.position*) modrm) #b00)
	     (if (or scale index)
		 (progn
		   ;; when using SIB modrm.RM need to be #b100
		   ;; OLD ->>		   (setf (ldb (byte *modrm.reg.rm.bits* *modrm.rm.position*) modrm) #b100)
		   (setf (ldb (byte *modrm.reg.rm.bits* modrm-operand-position) modrm) #b100)
		   (destructuring-bind (rex sib) (encode-sib rex 0 effective-addr-operand)
		     (list rex modrm sib displacement)))
		 ;; no SIB
		 (progn
		   (setf (ldb (byte *modrm.reg.rm.bits* modrm-operand-position) modrm) (get-register-bits base))
		   (when (extended-register? base)
		     (setf (ldb (byte *rex.extension.bits* (get-rex-extension-bit-for-modrm modrm-operand-position)) rex) #b1))
		   (list rex modrm sib displacement))))
	    ((eq (number-type displacement) 'byte)
	     ;; one byte displacement
	     (setf (ldb (byte *modrm.mod.bits* *modrm.mod.position*) modrm) #b01)
	     (if (or scale index)
		 (progn
		   (setf (ldb (byte *modrm.reg.rm.bits* *modrm.rm.position*) modrm) #b100)
		   (destructuring-bind (rex sib) (encode-sib rex 0 effective-addr-operand)
		     (list rex modrm sib displacement))))
	     (progn
	       (setf (ldb (byte *modrm.reg.rm.bits* modrm-operand-position) modrm) (get-register-bits base))
	       (when (extended-register? base)
		 (setf (ldb (byte *rex.extension.bits* (get-rex-extension-bit-for-modrm modrm-operand-position)) rex) #b1))	       
	       (list rex modrm sib displacement)))
	    ((eq (number-type displacement) 'dword)
	     ;; dword (32bit) displacement
	     (setf (ldb (byte *modrm.mod.bits* *modrm.mod.position*) modrm) #b10)
	     (if (or scale index)
		 (progn
		   (setf (ldb (byte *modrm.reg.rm.bits* *modrm.rm.position*) modrm) #b100)
		   (destructuring-bind (rex sib) (encode-sib rex 0 effective-addr-operand)
		     (list rex modrm sib displacement)))
		 (progn
		   (setf (ldb (byte *modrm.reg.rm.bits* modrm-operand-position) modrm) (get-register-bits base))
		   (when (extended-register? base)
		     (setf (ldb (byte *rex.extension.bits* (get-rex-extension-bit-for-modrm modrm-operand-position)) rex) #b1))		   
		   (list rex modrm sib displacement))))
	    (t (error (format nil "Unknown combo")))))))

(defun encode-sib (rex sib effective-addr-operand)
  (destructuring-bind (base index scale displacement) effective-addr-operand
    (declare (ignore displacement))
    (if base
	(progn
	  (setf (ldb (byte *sib.base.bits* *sib.base.position*) sib) (get-register-bits base))
	  (when (extended-register? base)
	    (setf (ldb (byte *rex.extension.bits* *rex.b.position*) rex) #b1)))
	(setf (ldb (byte *sib.base.bits* *sib.base.position*) sib) #b101))
    (if index
	(progn
	  (setf (ldb (byte *sib.index.bits* *sib.index.position*) sib) (get-register-bits index))
	  (when (extended-register? index)
	    (setf (ldb (byte *rex.extension.bits* *rex.x.position*) rex) #b1)))
	(setf (ldb (byte *sib.index.bits* *sib.index.position*) sib) #b100))
    (setf (ldb (byte *sib.scale.bits* *sib.scale.position*) sib) (get-scale-bits scale))
    (list rex sib)))

#+nil
(defun encode-one-operand-direct (operand template-operand template-flags template-prefixes template-rex template-opcode template-modrm)
  (declare (ignore template-operand))
  (let ((r-extended-register (extended-register? operand)))
    (when r-extended-register
      (unless template-rex
	(setf template-rex 0))
      ;; mark REX for R8-R15 registers
      (setf (ldb (byte rex.r.size rex.r.position) template-rex) #b1))
    (if (contain-flag 'register-added-to-opcode template-flags)
	;; add register value to opcode
	(setf (ldb (byte 3 0) template-opcode) (get-register-bits operand))
	;; encode register in modrm
	(progn
	  (unless template-modrm
	    (setf template-modrm 0))
	  (setf (ldb (byte modrm.reg.size modrm.reg.position) template-modrm) (get-register-bits operand))))
    (list template-prefixes template-rex template-opcode template-modrm)))


(defun encode-one-operand-instruction (mnemonic operands)
  (let* ((template (find-instruction-template mnemonic operands))
	 (template-operand (first (inst-template-operands template)))
	 (operand (first operands))
	 (template-flags (inst-template-flags template))
	 (template-prefixes (inst-template-prefixes template))
	 (template-rex (inst-template-rex template))
	 (template-opcode (inst-template-opcode template))
	 (template-modrm (inst-template-modrm template)))))


(defun encode-two-operand-instruction (mnemonic operands)
  (let* ((template (find-instruction-template mnemonic operands))
	 (dest-operand (first operands))
	 (source-operand (second operands))
	 (template-dest-operand (first (inst-template-operands template)))
	 (template-source-operand (second (inst-template-operands template)))
	 (template-flags (inst-template-flags template))
	 (template-prefixes (inst-template-prefixes template ))
	 (template-rex (inst-template-rex template))
	 (template-opcode (inst-template-opcode template))
	 (template-modrm (inst-template-modrm template))
	 (template-opcode-d-bit (opcode-d-bit template-opcode)))
    (destructuring-bind (rex modrm sib displacement)
	(encode-operands template-rex (or template-modrm 0) dest-operand source-operand template-dest-operand template-source-operand template-opcode-d-bit)
      (list template-prefixes rex template-opcode modrm sib displacement))))

(defun effective-address? (operand)
  (when (listp operand) operand))

(defun check-instruction-format (inst args)
  ;; TODO
  (declare (ignorable inst args))
  t)

(defun encode-instruction (mnemonic operands)
  (if (= (length operands) 1)
      (encode-one-operand-instruction mnemonic operands)
      (encode-two-operand-instruction mnemonic operands)))

(defun @ (base index scale displacement)
    (list base index scale displacement))

(defun inst (inst &rest args)
  (encode-instruction inst args))


;;; instruction templates

(define-inst-template :push (:reg) (register-added-to-opcode) 
		      nil nil #x50 nil)

(define-inst-template :push (:imm) ()
		      nil nil #x68 nil)

(define-inst-template :push (:addr) ()
		      nil nil #xff #b00110000)

(define-inst-template :pop (:reg) (register-added-to-opcode)
		      nil nil #x58 nil)

(define-inst-template :mov (:reg :reg) ()
		      nil #x48 #x89 nil)

(define-inst-template :mov (:addr :reg) ()
		      nil #x48 #x89 nil)



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
