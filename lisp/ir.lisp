(in-package #:clcomp)

(defparameter *temp-counter* 0)
(defparameter *label-counter* 0)

(defstruct ir-component code constants fixups)

(defparameter *special* nil)

(defun normalize (ir-component)
  (make-ir-component :code (reverse (ir-component-code ir-component))
		     :constants (ir-component-constants ir-component)))

(defun special? (symbol)
  (member symbol *special*))

;;; FIXME, use structures
(defun make-temp-location ()
  (incf *temp-counter*)
  (intern (concatenate 'string "TEMP-" (write-to-string *temp-counter*))))

(defun make-label ()
  (incf *label-counter*)
  (intern (concatenate 'string "LABEL-" (write-to-string *label-counter*))))

(defun emit-ir (component ir)
  (push ir (ir-component-code component)))

(defun emit-constant-ir (component node)
  (let ((location (make-temp-location)))
   (emit-ir component (list 'load-const location (immediate-constant-node-value node)))
   location))

;;; FIXME, this is not right
(defun emit-quoted-form-ir (component node)
  (let ((location (make-temp-location)))
   (emit-ir component (list 'load-quoted location (quoted-node-form node)))
   location))

(defun emit-progn-ir (component node)
  (let ((nodes (progn-node-forms node))
	(location (make-temp-location))
	(last-location))
    (dolist (node nodes)
      (setf last-location (emit-node-ir component node)))
    (emit-ir component (list 'load location last-location))
    location))

(defun emit-let-binding (component binding)
  (let ((l-value (lexical-var-node-name binding))
	(r-form (lexical-var-node-form binding)))
    (emit-ir component (list 'load l-value (emit-node-ir component r-form)))))

(defun emit-let-ir (component node)
  (let ((bindings (let-node-bindings node))
	(form (let-node-form node))
	(location (make-temp-location)))
    (dolist (binding bindings)
      (emit-let-binding component binding))
    (emit-node-ir component form)
    location))

(defun emit-if-ir (component node)
  (let ((test-node (if-node-test-form node))
	(true-node (if-node-true-form node))
	(false-node (if-node-false-form node))
	(true-label (make-label))
	(exit-label (make-label))
	(location (make-temp-location)))
    (let ((test-location (emit-node-ir component test-node)))
      (emit-ir component (list 'if test-location 'go true-label))
      (emit-ir component (list 'load location (emit-node-ir component false-node)))
      (emit-ir component (list 'go exit-label))
      (emit-ir component (list 'label true-label))
      (emit-ir component (list 'load location (emit-node-ir component true-node)))
      (emit-ir component (list 'label exit-label))
      location)))

(defun emit-argument-ir (component argument)
  (let ((location (make-temp-location)))
    (emit-ir component
	     (list 'load location
		   (emit-node-ir component argument)))
    location))

(defun emit-call-ir (component node)
  (let ((fun (call-node-function node))
	(arguments (call-node-arguments node))
	(location (make-temp-location)))
    (let ((arg-locations))
      (dolist (argument arguments)
	(push (emit-argument-ir component argument) arg-locations))
      (dolist (arg-location arg-locations)
	(emit-ir component (list 'param arg-location))))
    (emit-ir component (list 'load location 'call fun))
    location))

(defun emit-node-ir (component node)
  (typecase node
    (lexical-var-node (lexical-var-node-name node))
    (progn-node (emit-progn-ir component node))
    (call-node (emit-call-ir component node))
    (if-node (emit-if-ir component node))
    (let-node (emit-let-ir component node))
    (lambda-node (let ((lambda-comp (make-ir-component))
		       (temp-loc (make-temp-location)))
		   (push (cons temp-loc lambda-comp) (ir-component-constants component))
		   (make-ir node lambda-comp temp-loc)))
    (quoted-node (emit-quoted-form-ir component node))
    (immediate-constant-node (emit-constant-ir component node))))

(defun make-ir (lambda-node &optional component name)
  (let ((component (or component (make-ir-component))))
    (let ((arguments (lambda-node-arguments lambda-node))
	  (body (lambda-node-body lambda-node)))
      (emit-ir component (list 'lambda-entry))
      (emit-ir component (list 'arg-check (length arguments)))
      (dolist (arg arguments)
	(emit-ir component (list 'receive-param (lexical-var-node-name arg))))
      (emit-ir component (list 'return (emit-node-ir component body))))
    (or name (normalize component))))

(defun print-ir (component)
  (dolist (inst (ir-component-code component))
    (print inst)))

(defun optimize-ir1 (ir)
  ir)
