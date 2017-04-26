(in-package #:clcomp)

(defparameter *stack-pointer-reg* :RSP)
(defparameter *base-pointer-reg* :RBP)
(defparameter *fun-address-reg* :RAX)
(defparameter *return-value-reg* :RBX)
(defparameter *fun-number-of-arguments-reg* :RCX)
(defparameter *heap-header-reg* :R15)
(defparameter *fun-arguments-regs* '(:RDX :RSI :RDI :R8))
(defparameter *scratch-regs* '(:R9 :R10 :R11 :R12))
(defparameter *preserved-regs* '(:R13 :R14))


(defparameter *segment-instructions* nil)

(defun try-constant-component-blocks-phase (constants)
  (dolist (constant constants)
    (when (eq (type-of (cdr constant)) 'ir-component)
      (component-blocks-phase (cdr constant)))))

(defun component-blocks-phase (ir-component)
  (let ((blocks (make-blocks (ir-component-code ir-component))))
    (setf (ir-component-code-blocks ir-component)
	  (remove-dead-blocks (connect-blocks blocks)))
    (try-constant-component-blocks-phase (ir-component-constants ir-component))
    (setf (ir-component-code ir-component) nil)
    ir-component))

(defun clcomp-compile (exp)
  (let* ((ir-component (make-ir (create-node (expand exp)))))
    (component-blocks-phase ir-component)))
