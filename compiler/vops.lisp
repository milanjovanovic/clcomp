(in-package :clcomp)

(declaim (optimize (speed 0) (safety 3) (debug 3)))


(defparameter *segment-instructions* nil)

(defun inst (&rest rest)
  (push rest *segment-instructions*))

(defparameter *known-vops* (make-hash-table))

(defparameter *c-call-save-registers* '(:rax :rbx :rcx :rdx :rsi :rdi :r8
					:r9 :r10 :r11 :r12 :r13 :r14 :r15))

(defstruct vop name arguments res fun)

(defun get-vop (name)
  (gethash name *known-vops*))

(defun get-args-count (vop)
  (length (vop-arguments vop)))

(defun get-args-types (vop)
  (mapcar 'second (vop-arguments vop)))

(defun get-res-type (vop)
  (second (vop-res vop)))

(defmacro define-vop (name
		      (&rest res)
		      (&rest arguments)
		      &body body)
  (let ((res (if (listp (car res))
		 res
		 (list res))))
    `(setf (gethash ',name *known-vops*)
	   (make-vop :name ',name
		     :res ',res
		     :arguments ',arguments
		     :fun (lambda ,(append (mapcar 'car res)
				    (append (mapcar 'car arguments)
				     (list '$stack-top-operand$)))
			    (declare (ignorable $stack-top-operand$))
			    (progn
			      ,@body))))))

(defun make-vop-label (name)
  (gensym name))

(defun inline-vop (vops &rest args)
  (dolist (i (get-vop-code (get-vop vops) args))
    (push i *segment-instructions*)))

(defun get-vop-code (vop  args)
  (let ((*segment-instructions* nil))
    (apply (vop-fun vop) args)
    (reverse *segment-instructions*)))

(defun operand-types-match (caller-operands vop-operands)
  (and
   (= (length caller-operands) (length vop-operands))
   (do ((cops caller-operands (cdr cops))
	(vops vop-operands (cdr vops)))
       ((null cops) t)
     (unless (find (car cops) (car vops))
       (return)))))

(defun find-vop (name result arguments)
  (let ((vop (get-vop name)))
    (when vop
      (let ((vop-args (vop-arguments vop))
	    (vop-result (vop-res vop)))
	(when (and (operand-types-match result vop-result)
		   (operand-types-match arguments vop-args))
	  vop)))))
