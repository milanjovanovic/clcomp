(in-package #:clcomp)

(declaim (optimize (speed 0) (debug 3)))

;;; BASIC ARCH DEFINITIONS

(defparameter *registers* (make-hash-table))
(defparameter *xmm-registers* (make-hash-table))

(defun defregister (registers register-bits extend-bit)
  (let ((bits 128))
    (dolist (register registers)
      (setf bits (/ bits 2))
      (setf (gethash register *registers*) (list register-bits extend-bit bits registers)))))

(defun get-register (register)
  (or (gethash register *registers*)
      (gethash register *xmm-registers*)))

(defun get-register-bits (register)
  (first (get-register register)))

(defun get-register-extend-bit (register)
  (second (get-register register)))

(defun get-register-size (register)
  (third (get-register register)))

(defun get-register-line (register)
  (fourth (get-register register)))

(defun is-register (what)
  (get-register what))

(defun extended-register? (register)
  (let ((extend-bit (get-register-extend-bit register)))
    (and extend-bit (= 1 extend-bit))))

(defun ea-has-extended-register? (effective-addres)
  (or (extended-register? (second effective-addres))
      (extended-register? (third effective-addres))))

(defun ea-address-size (ea)
  (first ea))

(defregister '(:rax :eax :ax :al) #b000 #b0)
(defregister '(:rcx :ecx :cx :cl) #b001 #b0)
(defregister '(:rdx :edx :dx :dl) #b010 #b0)
(defregister '(:rbx :ebx :bx :bl) #b011 #b0)
(defregister '(:rsp :esp :sp :ah) #b100 #b0)
(defregister '(:rbp :ebp :bp :ch) #b101 #b0)
(defregister '(:rsi :esi :si :dh) #b110 #b0)
(defregister '(:rdi :edi :di :bh) #b111 #b0)
(defregister '(:r8 :r8d :r8w :r8L) #b000 #b1)
(defregister '(:r9 :r9d :r9w :r9L) #b001 #b1)
(defregister '(:r10 :r10d :r10w :r10l) #b010 #b1)
(defregister '(:r11 :r11d :r11w :r11l) #b011 #b1)
(defregister '(:r12 :r12d :r12w :r12l) #b100 #b1)
(defregister '(:r13 :r13d :r13w :r13l) #b101 #b1)
(defregister '(:r14 :r14d :r14w :r14l) #b110 #b1)
(defregister '(:r15 :r15d :r15w :r15l) #b111 #b1)


(defun def-xmm-register (register register-bits extend-bit)
  (setf (gethash  register *xmm-registers*) (list register-bits extend-bit)))

(defun get-xmm-register (register)
  (gethash register *xmm-registers*))

(def-xmm-register :xmm0 #b000 #b0)
(def-xmm-register :xmm1 #b001 #b0)
(def-xmm-register :xmm2 #b010 #b0)
(def-xmm-register :xmm3 #b011 #b0)
(def-xmm-register :xmm4 #b100 #b0)
(def-xmm-register :xmm5 #b101 #b0)
(def-xmm-register :xmm6 #b110 #b0)
(def-xmm-register :xmm7 #b111 #b0)
(def-xmm-register :xmm8 #b000 #b1)
(def-xmm-register :xmm9 #b001 #b1)
(def-xmm-register :xmm10 #b010 #b1)
(def-xmm-register :xmm11 #b011 #b1)
(def-xmm-register :xmm12 #b100 #b1)
(def-xmm-register :xmm13 #b101 #b1)
(def-xmm-register :xmm14 #b110 #b1)
(def-xmm-register :xmm15 #b111 #b1)




;;; for runtime register selecting
;;;
(defun %q (reg)
  (first (get-register-line reg)))
(defun %d (reg)
  (second (get-register-line reg)))
(defun %w (reg)
  (third (get-register-line reg)))
(defun %b (reg)
  (fourth (get-register-line reg)))

(defparameter *instruction-pointer-register* :rip)
(defun ip-register? (register)
  (eq register *instruction-pointer-register*))

(defun rbp-or-r13 (reg)
  ;; fixme, 32bit registers ?
  (or (eq reg :rbp)
      (eq reg :r13)))

(defun rsp-or-r12 (reg)
  ;; fixme. 32bit registers ?
  (or (eq reg :rsp)
      (eq reg :r12)))

(defparameter *tmpl-reg-operands* '(:reg8 :reg16 :reg32 :reg64 :xmmreg))

(defun register-type (register)
  (let ((register-bits (get-register-size register))
	(xmm-reg-config (get-xmm-register register)))
    (if register-bits
	(case register-bits
	  (8 :reg8)
	  (16 :reg16)
	  (32 :reg32)
	  (64 :reg64))
	(when xmm-reg-config
	  :xmmreg))))

(defun is-64bit (register-operand)
  (eq :reg64 (register-type register-operand)))

(defun register-template-operand? (template-operand)
  (find template-operand *tmpl-reg-operands*))

(defparameter *tmpl-imm-operands* '(:imm8 :imm16 :imm32 :imm64
				    :uimm8 :uimm16 :uimm32 :uim64))

(defun unsigned-immediate-template-p (template)
  (member template '(:uimm8 :uimm16 :uimm32 :uim64) ))

(defun get-immediate-type (number unsigned)
  (if unsigned
      (unsigned-number-type number)
      (signed-number-type number)))

(defun immediate-type (number)
  (let ((type (signed-number-type number)))
    (case type
      (byte :imm8)
      (word :imm16)
      (dword :imm32)
      (qword :imm64)
      (t :too-large))))

(defun immediate-bits (type)
  (ecase type
    ((:uimm8 :imm8) 8)
    ((:uimm16 :imm16) 16)
    ((:uimm32 :imm32) 32)
    ((:uimm64 :imm64) 64)))

(defun register-template-to-byte (reg)
  (case reg
    (:reg8 :byte)
    (:reg16 :word)
    (:reg32 :dword)
    (:reg64 :qword)))

(defun address-bits (address)
  (case address
    (:addr8 8)
    (:addr16 16)
    (:addr32 32)
    (:addr64 64)))

(defun nbyte-bits (size)
  (ecase size
    ((byte :byte)  8)
    ((word :word)  16)
    ((dword :dword) 32)
    ((qword :qword) 64)))

(defun is-nbyte-descriptor (what)
  (member what '(:byte :word :dword :qword :xmmword)))

(defun imm-is-of-type (immediate type)
  (let ((unsigned-template (unsigned-immediate-template-p type)))
    (<= (nbyte-bits (get-immediate-type immediate unsigned-template))
	(immediate-bits type))))

(defun immediate-template-operand? (template-operand)
  (find template-operand *tmpl-imm-operands*))

(defun immediate-operand? (operand)
  (numberp operand))

;;; basic encoding details

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

(defparameter *rex.b.byte* (byte 1 0))
(defparameter *rex.x.byte* (byte 1 1))
(defparameter *rex.r.byte* (byte 1 2))
(defparameter *rex.w.byte* (byte 1 3))

(defparameter *rex.b.position* 0)
(defparameter *rex.x.position* 1)
(defparameter *rex.r.position* 2)
(defparameter *rex.w.position* 3)
(defparameter *rex.extension.bits* 1)

(defun modrm (mod reg rm)
  (let ((modrm 0))
    (setf (ldb (byte 2 6) modrm) mod)
    (setf (ldb (byte 3 3) modrm) reg) ;; default destination operand
    (setf (ldb (byte 3 0) modrm) rm) ;; default source operand
    modrm))

(defparameter *modrm.mod.byte* (byte 2 6))
(defparameter *modrm.reg.byte* (byte 3 3))
(defparameter *modrm.rm.byte* (byte 3 0))

(defparameter *modrm.mod.position* 6)
(defparameter *modrm.mod.bits* 2)
(defparameter *modrm.reg.position* 3)
(defparameter *modrm.rm.position* 0)
(defparameter *modrm.reg.rm.bits* 3)

;;; Based on modrm position for register get rex bit position that extend register in modrm
(defun rex-ext-byte (modrm-position)
  (cond ((= modrm-position *modrm.rm.position*) (byte 1 *rex.b.position*))
	((= modrm-position *modrm.reg.position*) (byte 1 *rex.r.position*))
	(t (error "Unknown position"))))


(defun get-scale-bits (scale)
  (if (null scale)
      #b00
      (cond ((= scale 1) #b00)
	    ((= scale 2) #b01)
	    ((= scale 4) #b10)
	    ((= scale 8) #b11)
	    (t (error (format nil "Unknown scale ~a" scale))))))

(defun sib (base index scale)
  ;; SIB is one byte
  ;; | scale 2 bits | index 3 bits | base 3 bits|
  (let ((sib 0))
    (setf (ldb (byte 2 6) sib) scale)
    (setf (ldb (byte 3 3) sib) index)
    (setf (ldb (byte 3 0) sib) base)
    sib))

(defparameter *sib.scale.byte* (byte 2 6))
(defparameter *sib.index.byte* (byte 3 3))
(defparameter *sib.base.byte* (byte 3 0))

(defparameter *sib.scale.position* 6)
(defparameter *sib.scale.bits* 2)
(defparameter *sib.index.position* 3)
(defparameter *sib.index.bits* 3)
(defparameter *sib.base.position* 0)
(defparameter *sib.base.bits* 3)


;;; end of basic definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *instructions* (make-hash-table))

(defun get-mnemonic-templates (mnemonic)
  (gethash mnemonic *instructions*))

(defun create-template (mnemonic operands flags prefixes rex opcode-prefix opcode modrm)
  (list mnemonic operands flags prefixes rex opcode-prefix opcode modrm))

(defun add-inst-template (mnemonic operands flags prefixes rex opcode-prefix opcode modrm)
  (let ((insts (gethash mnemonic *instructions*)))
    (setf (gethash mnemonic *instructions*)
	  (cons (create-template mnemonic operands flags prefixes rex opcode-prefix opcode modrm)
		insts))))

(defmacro define-inst-template (mnemonic
				(&rest operands)
				flags
				prefixes
				opcode-prefix
				opcode
				modrm)
  `(add-inst-template ,mnemonic ',operands ',flags ,prefixes nil ,opcode-prefix ,opcode ,modrm))

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
(defun inst-template-opcode-prefix (inst-template)
  (nth 5 inst-template))
(defun inst-template-opcode (inst-template)
  (nth 6 inst-template))
(defun inst-template-modrm (inst-template)
  (nth 7 inst-template))

(defun contain-flag (flag flags)
  (find flag flags))

(defun op-address? (operand)
  (when (consp operand) operand))

(defun tmpl-op-address? (template-operand)
  (if (consp template-operand)
      (intersection template-operand '(:addr8 :addr16 :addr32 :addr64 :addr :xmmaddr))
      (member template-operand '(:addr8 :addr16 :addr32 :addr64 :addr :xmmaddr))))

(defun match-immediate-type (type operand)
  (and (numberp operand)
       (if (member type '(:imm8 :imm16 :imm32 :imm64))
	   (imm-is-of-type operand type)
	   (imm-is-of-type operand type))))

(defun match-type (type operand)
  (and
   (cond ((register-template-operand? type)
	  (eq (register-type operand) type))
	 ((eq type :addr)
	  (and (consp operand)
	       (eq (first operand) :qword)))
	 ((member type '(:addr8 :addr16 :addr32 :addr64))
	  (and (typep operand 'list)
	       (= (length operand) 5)
	       (= (address-bits type)
		  (nbyte-bits (first operand)))))
	 ((eq type :xmmaddr)
	  (and (typep operand 'list)
	       (= (length operand) 5)
	       (eq (first operand) :xmmword)))
	 ((immediate-template-operand? type)
	  (and (typep operand 'number)
	       (imm-is-of-type operand type)))
	 (t (eq type operand))) ; this matches literal register in template
   type))

;;; If template operand is list of more than one type find the one that best match actual operand
(defun find-best-match-type (template-operand operand)
  (if (and (null template-operand) (null operand))
      nil
      (let ((last-matched-type nil)
	    (last-score 100))
	(if (listp template-operand)
	    (dolist (type template-operand)
	      (let ((match-value (operand-matching-value type operand)))
		(when (< match-value last-score)
		  (setf last-matched-type type)
		  (setf last-score match-value))))
	    (setf last-matched-type template-operand))
	last-matched-type)))

;;; FIXME
;;; do we want to match immediate values with right match based on immediate size
;;; or we want to match with first template immediate operand that can hold operand
;;; this can make bugs in assembler if we don't have inst defined for operand size
;;; P.S. This only works for immediate operands, register operand need to fully match size with template operand
(defun operand-matching-value (template-operand operand)
  (cond ((and (null template-operand)
	      (null operand))
	 0)
	((or (register-template-operand? template-operand)
	     (is-register template-operand))
	 (if (is-register operand)
	     0
	     10))
	((member template-operand '(:addr :addr64 :addr32 :addr16 :addr8 :xmmaddr))
	 (if (op-address? operand)
	     0
	     10))
	((immediate-template-operand? template-operand)
	 (let ((unsigned-template (unsigned-immediate-template-p template-operand)))
	   (- (immediate-bits template-operand)
	      (nbyte-bits (get-immediate-type operand unsigned-template)))))))

(defun match-any-of-template-operands (template-operand operand)
  (dolist (template-type template-operand)
    (when (match-type template-type operand )
      (return-from match-any-of-template-operands t))))

;; in template operand we can have list of operand types that match
(defun match-instruction-operand-type (template-operand operand)
  (cond ((listp template-operand)
	 (match-any-of-template-operands template-operand operand))
	(t (match-type template-operand operand))))

(defun match-operands-type (template-operands operands)
  (and (match-instruction-operand-type (first template-operands) (first operands))
       (or (and (null (second template-operands))
		(null (second operands)))
	   (match-instruction-operand-type (second template-operands) (second operands)))))

(defun reformat-list-template-operands (template operands)
  (let* ((template-operands (inst-template-operands template))
	 (tpl-first-operand (first template-operands))
	 (tpl-second-operand (second template-operands))
	 (match-tpl-first (find-best-match-type tpl-first-operand (first operands)))
	 (match-tpl-second (find-best-match-type tpl-second-operand (second operands))))
    (create-template (inst-template-mnemonic template)
		     (cons match-tpl-first
			   (if match-tpl-second (list match-tpl-second) nil))
		     (inst-template-flags template)
		     (inst-template-prefixes template)
		     (inst-template-rex template)
		     (inst-template-opcode-prefix template)
		     (inst-template-opcode template)
		     (inst-template-modrm template))))

(defun sort-matched-templates (matched-templates operands)
  (let ((last-template nil)
	(last-matched-value 10000)) ;; some big number
    (dolist (template matched-templates)
      (let* ((templ-operands (inst-template-operands template))
	     (match-value (+ (operand-matching-value (find-best-match-type (first templ-operands) (first operands))
						     (first operands))
			     (operand-matching-value (find-best-match-type (second templ-operands) (second operands))
						     (second operands)))))
	(when (< match-value last-matched-value)
	  (setf last-template template)
	  (setf last-matched-value match-value))))
    last-template))

(defun best-template-match (matched-templates operands)
  (cond ((null matched-templates) nil)
	((= 1 (length matched-templates)) (first matched-templates))
	(t (sort-matched-templates matched-templates operands))))

(defun match-mnemonic-operands (mnemonic-templates operands)
  (let ((operands-count (length operands))
	(matched))
    (dolist (template mnemonic-templates)
      (let ((tmpl-operands-count (length (inst-template-operands template))))
	(when (and (= tmpl-operands-count operands-count)
		   (match-operands-type (inst-template-operands template) operands))
	  (push template matched))))
    (let ((template (best-template-match matched operands)))
      (if (null template)
	  (error "Can't find template for instruction")
	  template))))

(defun find-instruction-template (mnemonic operands)
  (let ((mnemonic-templates (get-mnemonic-templates mnemonic)))
    (unless mnemonic-templates
      (error (format nil "Unknown instruction ~A" mnemonic)))
    (if (zerop (length operands))
	(first mnemonic-templates)
     (or (match-mnemonic-operands mnemonic-templates operands)
	 (error (format nil "Unknown instruction ~A with arguments ~A" mnemonic operands))))))

(defun get-modrm-operand-positions (template-source-operand template-dest-operand)
  (let* ((source-is-r/m (tmpl-op-address? template-source-operand))
	 (dest-is-r/m (tmpl-op-address? template-dest-operand)))
    (if (or source-is-r/m dest-is-r/m)
	(if source-is-r/m
	    (list *modrm.rm.position* *modrm.reg.position*)
	    (list *modrm.reg.position* *modrm.rm.position*))
	(list *modrm.reg.position* *modrm.rm.position*))))

(defun encode-sib (rex sib effective-addr-operand)
  (destructuring-bind (size base index scale displacement) effective-addr-operand
    (declare (ignore displacement size))
    (if base
	(progn
	  (setf (ldb *sib.base.byte* sib) (get-register-bits base))
	  (when (extended-register? base)
	    (setf rex (or rex #x40))
	    (setf (ldb *rex.b.byte* rex) #b1)))
	(setf (ldb *sib.base.byte* sib) #b101))
    (if index
	(progn
	  (setf (ldb *sib.index.byte* sib) (get-register-bits index))
	  (when (extended-register? index)
	    (setf rex (or rex #x40))
	    (setf (ldb *rex.x.byte* rex) #b1)))
	(setf (ldb *sib.index.byte* sib) #b100))
    (setf (ldb *sib.scale.byte* sib) (get-scale-bits scale))
    (list rex sib)))

(defun encode-effective-memory-address (rex modrm addr-operand)
  (let ((sib nil))
    (destructuring-bind (size base index scale displacement) addr-operand
      (declare (ignore size))
      ;; special case for rbp and r13
      ;; Table 2-5. Special Cases of REX Encodings
      (when (and (rbp-or-r13 base) (null displacement))
	(setf displacement 0))
      (cond
	;; if base register is instruction pointer we don't need SIB encoding
	;; can't have index or scale when base is instruction pointer register
	((and base (ip-register? base))
	 (setf (ldb *modrm.mod.byte* modrm) #b00)
	 (setf (ldb *modrm.rm.byte* modrm) #b101)
	 (list rex modrm sib (dword-as-byte-list (or displacement 0))))
	    
	;; when base register is null displacement as dword needs to be encoded
	((not base)
	 ;; we can't have base-scale combo, only index-scale
	 ;; FIXME, we should check instruction format before encoding it
	 ;; when using SIB modrm.RM need to be #b100
	 (setf (ldb *modrm.rm.byte*  modrm) #b100)
	 (destructuring-bind (rex sib) (encode-sib rex 0 addr-operand)
	   (list rex modrm sib (dword-as-byte-list (make-signed-dword (or displacement 0))))))

	;; no displacement but we have base register
	((null displacement)
	 (setf (ldb *modrm.mod.byte* modrm) #b00)
	 ;; need to encode sib when base ir rsp or r12
	 ;; Table 2-5. Special Cases of REX Encodings
	 (if (or scale index (rsp-or-r12 base))
	     (progn
	       ;; when using SIB modrm.RM need to be #b100
	       (setf (ldb *modrm.rm.byte* modrm) #b100)
	       (destructuring-bind (rex sib) (encode-sib rex 0 addr-operand)
		 (list rex modrm sib displacement)))
	     ;; no SIB
	     (progn
	       (setf (ldb *modrm.rm.byte* modrm) (get-register-bits base))
	       (when (extended-register? base)
		 (setf rex (or rex *rex*))
		 (setf (ldb (rex-ext-byte *modrm.rm.position*) rex) #b1))
	       (list rex modrm sib displacement))))
	
	;; we have displacement, displacement is signed
	(t (let ((displacement-type (signed-number-type displacement)))
	     (cond ((eq displacement-type 'byte)
		    ;; one byte displacement
		    (setf displacement (byte-as-byte-list (make-signed-byte displacement)))
		    (setf (ldb *modrm.mod.byte* modrm) #b01))
		   ((or (eq displacement-type 'word)
			(eq displacement-type 'dword))
		    ;; dword (4 bytes) displacement
		    (setf displacement (dword-as-byte-list (make-signed-dword displacement)))
		    (setf (ldb *modrm.mod.byte* modrm) #b10))
		   (t (error "Bad displacement")))
	     (if (or scale index (rsp-or-r12 base))
		 (progn
		   (setf (ldb *modrm.rm.byte* modrm) #b100)
		   (destructuring-bind (rex sib) (encode-sib rex 0 addr-operand)
		     (list rex modrm sib displacement)))
		 (progn
		   (setf (ldb *modrm.rm.byte* modrm) (get-register-bits base))
		   (when (extended-register? base)
		     (setf (ldb (rex-ext-byte *modrm.rm.position*) rex) #b1))	       
		   (list rex modrm sib displacement)))))))))

(defun encode-operands (rex modrm dest-operand source-operand template-dest-operand template-source-operand opcode flags
			matching-template-dest-operand matching-template-source-operand)
  (let ((immediate nil))
    (destructuring-bind (source-operand-modrm-position dest-operand-modrm-position)
	(get-modrm-operand-positions template-source-operand template-dest-operand)
      (let ((rex-source-extend (rex-ext-byte source-operand-modrm-position))
	    (rex-dest-extend (rex-ext-byte dest-operand-modrm-position)))
	(cond ((register-template-operand? matching-template-source-operand)
	       (setf modrm (or modrm 0))
	       (setf (ldb (byte *modrm.reg.rm.bits* source-operand-modrm-position) modrm) (get-register-bits source-operand))
	       (when (and (eq :reg64 matching-template-source-operand)
			  (not (contain-flag '+v64 flags)))
		 (setf rex (or rex *rex*))
		 (setf (ldb *rex.w.byte* rex) #b1))
	       (when (extended-register? source-operand)
		 (setf rex (or rex *rex*))
		 (setf (ldb rex-source-extend rex) #b1))))
	(cond ((register-template-operand? matching-template-dest-operand)
	       (if (contain-flag '+r flags)
		   (setf (ldb (byte 3 0) opcode) (get-register-bits dest-operand))
		   (progn
		     (setf modrm (or modrm 0))
		     (setf (ldb (byte *modrm.reg.rm.bits* dest-operand-modrm-position) modrm) (get-register-bits dest-operand))))
	       (when (and (eq :reg64 matching-template-dest-operand)
			  (not (contain-flag '+v64 flags)))
		 (setf rex (or rex *rex*))
		 (setf (ldb *rex.w.byte* rex) #b1))
	       (when (extended-register? dest-operand)
		 (setf rex (or rex *rex*))
		 (setf (ldb rex-dest-extend rex) #b1))))
	(cond ((immediate-template-operand? matching-template-source-operand)
	       (setf immediate (immediate-as-byte-list source-operand matching-template-source-operand))))
	(cond ((immediate-template-operand? matching-template-dest-operand)
	       (setf immediate (immediate-as-byte-list dest-operand matching-template-dest-operand))))
	(let ((addr-operand (or (op-address? source-operand)
				(op-address? dest-operand))))
	  (if addr-operand
	      (progn
		(when (and (eq (ea-address-size addr-operand) :qword)
			   (not (eq (ea-address-size addr-operand) :xmmword))
			   (not (contain-flag '+v64 flags)))
		  (setf rex (or rex *rex*))
		  (setf (ldb *rex.w.byte* rex) #b1))		
		(destructuring-bind (rex modrm sib displacement) (encode-effective-memory-address rex modrm addr-operand)
		  (list opcode rex modrm sib displacement immediate)))
	      (progn
		(when modrm
		  (setf (ldb *modrm.mod.byte* modrm) #b11))
		(list opcode rex modrm nil nil immediate))))))))

(defun encode-one-operand-mnemonic (prefixes rex opcode modrm operand template-operand flags matching-template-operand)
  (declare (ignore template-operand))
  (cond ((register-template-operand? matching-template-operand)
	 (when (and (eq :reg64 matching-template-operand)
		    (not (contain-flag '+v64 flags)))
	   (setf rex (or rex *rex*))
	   (setf (ldb *rex.w.byte* rex) #b1))
	 (when (extended-register? operand)
	   (setf rex (or rex *rex*))
	   (setf (ldb *rex.b.byte* rex) #b1))
	 (if (contain-flag '+r flags)
	     (setf (ldb (byte 3 0) opcode) (get-register-bits operand))
	     (progn
	       (setf modrm (or modrm 0))
	       (setf (ldb *modrm.mod.byte* modrm) #b11)
	       (setf (ldb (byte *modrm.reg.rm.bits* *modrm.rm.position*) modrm) (get-register-bits operand))))
	 (list prefixes rex opcode modrm nil nil nil))
	((op-address? operand)
	 (let ((rex (or rex (rex #b0 #b0 #b0 #b0))))
	   (when (and (eq (ea-address-size operand) :qword)
		      (not (eq (ea-address-size operand) :xmmword))
	   	      (not (contain-flag '+v64 flags)))
	     (setf (ldb *rex.w.byte* rex) #b1))
	   (destructuring-bind (rex modrm sib displacement) (encode-effective-memory-address rex modrm operand)
	     (let ((rex (if (ea-has-extended-register? operand) rex nil)))
	       (list prefixes rex opcode modrm sib displacement nil)))))
	((immediate-template-operand? matching-template-operand)
	 (list prefixes nil nil opcode nil nil (immediate-as-byte-list operand matching-template-operand)))))

(defun encode-one-operand-instruction (mnemonic operands)
  (let* ((template (find-instruction-template mnemonic operands))
	 (formatted-template (reformat-list-template-operands template operands))
	 (matching-template-operand (first (inst-template-operands formatted-template)))
	 (template-operand (first (inst-template-operands template)))
	 (operand (first operands))
	 (template-flags (inst-template-flags template))
	 (template-prefixes (inst-template-prefixes template))
	 (template-rex (inst-template-rex template))
	 (template-opcode-prefix (inst-template-opcode-prefix template))
	 (template-opcode (inst-template-opcode template))
	 (template-modrm (inst-template-modrm template)))
    (destructuring-bind (prefix rex opcode modrm sib displacement immediate)
	(encode-one-operand-mnemonic template-prefixes template-rex template-opcode template-modrm operand
				     template-operand template-flags matching-template-operand)
      (filter (append (list prefix rex template-opcode-prefix opcode modrm sib)
		      (reverse displacement)
		      (reverse immediate))
	      nil))))

(defun encode-two-operand-instruction (mnemonic operands)
  (let* ((template (find-instruction-template mnemonic operands))
	 (formatted-template (reformat-list-template-operands template operands))
	 (matching-template-dest-operand (first (inst-template-operands formatted-template)))
	 (matching-template-source-operand (second (inst-template-operands formatted-template)))
	 (dest-operand (first operands))
	 (source-operand (second operands))
	 (template-dest-operand (first (inst-template-operands template)))
	 (template-source-operand (second (inst-template-operands template)))
	 (template-flags (inst-template-flags template))
	 (template-prefix (inst-template-prefixes template ))
	 (template-rex (inst-template-rex template))
	 (template-opcode-prefix (inst-template-opcode-prefix template))
	 (template-opcode (inst-template-opcode template))
	 (template-modrm (inst-template-modrm template))
	 (template-opcode-d-bit (opcode-d-bit template-opcode))) ; not using this currently
    (declare (ignore template-opcode-d-bit))
    (destructuring-bind (opcode rex modrm sib displacement immediate) (encode-operands template-rex
										       template-modrm
										       dest-operand
										       source-operand
										       template-dest-operand
										       template-source-operand
										       template-opcode
										       template-flags
										       matching-template-dest-operand
										       matching-template-source-operand)
      (filter (append (list template-prefix rex template-opcode-prefix opcode modrm sib)
		      (reverse displacement)
		      (reverse immediate))
	      nil))))

(defun encode-no-operand-instruction (mnemonic operands)
  (let ((template (find-instruction-template mnemonic operands)))
    (if (consp (inst-template-opcode template))
	(inst-template-opcode template)
	(list (inst-template-opcode template)))))

(defun reformat-3size-addr (addr)
  (let ((first (first addr))
	(second (second addr))
	(third (third addr)))
    (cond ((and (is-register first)
		(is-register second)
		(typep third 'number))
	   (list first second third nil))
	  ((and (typep third 'number)
		(typep second 'number)
		(is-register first))
	   (list nil first second third))
	  (t (error "Bad addr format")))))

(defun reformat-2size-addr (addr)
  (let ((first (first addr))
	(second (second addr)))
    (cond ((and (is-register first)
		(is-register second))
	   (list first second nil nil))
	  ((and (is-register first)
		(typep second 'number))
	   (list nil first second nil))
	  (t (error "Bad addr format")))))

(defun reformat-1size-addr (addr)
  (let ((first (first addr)))
    (cond ((is-register first) (list first nil nil nil))
	  ((typep first 'number) (list nil nil nil first)))))

(defun get-addr-type (addr)
  (dolist (r addr)
    (when (is-register r)
      (return (register-template-to-byte (register-type r))))))

;;; FIXME, check for register type, if not register set it to QWORD
(defun maybe-insert-addr-size (addr)
  (if (and (consp addr)
	   (< (length addr) 5)
	   (not (is-nbyte-descriptor (first addr))))
      (cons (or (get-addr-type addr) :qword) addr)
      addr))

(defun reformat-operand (addr)
  (if (listp addr)
      (let* ((addr (maybe-insert-addr-size addr))
	     (size (length addr)))
	(cond ((= 5 size) addr)
	      ((= 4 size) (cons (first addr)
				(reformat-3size-addr (rest addr))))
	      ((= 3 size) (cons (first addr)
				(reformat-2size-addr (rest addr))))
	      ((= 2 size) (cons (first addr)
				(reformat-1size-addr (rest addr))))	      
	      (t (error "Bad addr format"))))
      addr))

(defun reparse-operands (operands)
  (let ((op-count (length operands)))
    (cond ((= op-count 2)
	   (list (reformat-operand (first operands)) (reformat-operand (second operands))))
	  ((= op-count 1)
	   (list (reformat-operand (first operands))))
	  ((= op-count 0)
	   operands)
	  (t (error "Wrong number of operands")))))

(defun encode-instruction (mnemonic operands)
  (let ((op-count (length operands)))
    (cond ((= op-count 2)
	   (encode-two-operand-instruction mnemonic operands))
	  ((= op-count 1)
	   (if (eq mnemonic :byte)
	       (list (first operands))
	       (encode-one-operand-instruction mnemonic operands)))
	  ((= op-count 0)
	   (encode-no-operand-instruction mnemonic operands))
	  (t (error "Wrong number of operands")))))

(defun @ (&rest rest)
    rest)

(defun make-instruction (&rest instruction)
  instruction)

(defun assemble-instruction (instruction)
  (encode-instruction (first instruction) (reparse-operands (rest instruction))))


;;; posible effective address format
;;; full format: (@ base index scale displacement)
;;; short: (@ base index scale)
;;;        (@ index scale displacement)
;;;        (@ base index)
;;;        (@ index scale)
;;;        (@ base)
;;;        (@ displacement)
