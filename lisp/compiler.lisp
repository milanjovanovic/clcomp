(in-package #:clcomp)

(defparameter *fun-address-reg* :RAX)
(defparameter *return-value-reg* :RBX)
(defparameter *fun-number-of-arguments-reg* :RCX)
(defparameter *heap-header-reg* :R15)
(defparameter *fun-arguments-regs* '(:RDX :RSI :RDI :R8))
(defparameter *scratch-regs* '(:R9 :R10 :R11 :R12))
(defparameter *preserved-regs* '(:R13 :R14))


(defparameter *segment-instructions* nil)

(defstruct translator registers locations code)

(defun try-constant-component-blocks-phase (constants)
  (dolist (constant constants)
    (when (eq (type-of (cdr constant)) 'ir-component)
      (component-blocks-phase (cdr constant)))))

(defun component-blocks-phase (ir-component)
  (let ((blocks (make-blocks (ir-component-code ir-component))))
    (setf (ir-component-code-blocks ir-component)
	  (fill-used-locations (remove-dead-blocks (connect-blocks blocks))))
    (try-constant-component-blocks-phase (ir-component-constants ir-component))
    (setf (ir-component-code ir-component) nil)
    ir-component))


;;;;;;;;;;;;;;;;;;;;;;
;;; ir to assembly

(defun emit-ir-assembly (asm translator)
  (push asm (translator-code translator)))

(defun translate-lambda-entry (translator)
  (emit-ir-assembly
   (list
    (list :push :RBP)
    (list :mov :RSP :RBP))
   translator))

(defun translate-params-count (translator ir)
  (let ((arguments-count (second ir)))
    (emit-ir-assembly
     (list
      (list :mov *fun-number-of-arguments-reg* arguments-count))
     translator)
    (when (> arguments-count (length *fun-arguments-regs*))
      (emit-ir-assembly
       (list (list :sub :RSP (* *word-size* (- arguments-count (length *fun-arguments-regs*)))))
       translator))))

(defun translate-receive-param (ir translator)
  ;; move arguments to scratch registers
  (declare (ignore ir translator)))

(defun translate-load (ir translator)
  (declare (ignore ir translator)))

(defun translate-load-return (ir translator)
  (declare (ignore ir translator)))

(defun translate-if (ir translator)
  (declare (ignore ir translator)))

(defun translate-go (ir translator)
  (declare (ignore ir translator)))

(defun translate-label (ir translator)
  (declare (ignore ir translator)))

(defun to-asm (ir translator)
  (let ((mnemonic (first ir)))
    (case mnemonic
      (lambda-entry (translate-lambda-entry translator))
      (params-count (translate-params-count ir translator))
      (receive-param (translate-receive-param ir translator))
      (load (translate-load ir translator))
      (load-return (translate-load-return ir translator))
      (if (translate-if ir translator))
      (fo (translate-go ir translator))
      (label (translate-label ir translator)))))

(defun translate-ir (ir-code)
  (let ((translator (make-translator :registers (make-hash-table)
				     :locations (make-hash-table))))
    (dolist (ir ir-code)
      (to-asm ir translator))))

(defun clcomp-compile (exp)
  (let* ((ir-component (make-ir (create-node (expand exp)))))
    (component-blocks-phase ir-component)
    (let ((ir-code (ir-component-code ir-component)))
      (translate-ir ir-code)
      ir-component)))
