(in-package #:clcomp)

(defun dump-hex-code (assembled)
  (apply #'concatenate 'string (mapcar #'byte-hex (reduce #'append assembled))))

(defun add-offset (hash offset)
  (maphash (lambda (k v)
	     (setf (gethash k hash) (+ v offset)))
	   hash))

;;; to simplify things we are encoding our jump as near jumps so they are always 6 bytes long
(defun set-jump-offsets (instructions direction)
  (let ((labels-offsets (make-hash-table))
	(new-instructions nil))
    (dolist (inst instructions)
      (cond ((eq (first inst) :label)
	     (setf (gethash (second inst) labels-offsets) 0)
	     (push inst new-instructions))
	    ((eq (first inst) :jump-fixup)
	     (let ((label-offset (gethash (third inst) labels-offsets)))
	       (if label-offset
		   (push (list (second inst)
			       (if (eq direction :normal)
				   ;; when we jump backward add jmp instruction size to jump offset
				   (- (+ label-offset (jump-instruction-size inst)))
				   label-offset))
			 new-instructions)
		   (push inst new-instructions))
	       (add-offset labels-offsets (jump-instruction-size inst))))
	    (t
	     (add-offset labels-offsets (instruction-size inst))
	     (push inst new-instructions))))
    new-instructions))

(defun is-unresolved-offset-memory-operand (operand)
  (and (consp operand)
       (let ((displacement (car (last operand))))
	 (and (consp displacement)
	      (eq (car displacement) 'displacement)
	      (second displacement)))))

(defun instruction-has-unresolved-memory-operand (instruction)
  (some #'is-unresolved-offset-memory-operand  (rest instruction)))

(defun unresolved-memory-operand-rip-name (instruction)
  (second (instruction-has-unresolved-memory-operand instruction)))

(defun jump-instruction-size (instruction)
  (let ((mnemonic (second instruction)))
    (cond ((eq mnemonic :jmp) 5)
	  (t 6))))

;;; just look at displacement and insert test value so we can get instruction size
(defun maybe-fix-ea-operand (operand &optional (displ 32)) ; use 32 as test displacement
  (let ((operand (reformat-operand operand)))
    (if (consp operand)
	(let ((displacement (car (last operand))))
	  (append (butlast operand)
		  (if (and (consp displacement)
			   (eq 'displacement (first displacement)))
		      (list displ)
		      (list displacement))))
	operand)))

(defun maybe-fix-unresolved-offset-instruction (instruction)
  (cons (first instruction)
	(mapcar #'maybe-fix-ea-operand (rest instruction))))

(defun resolve-instruciton-offset (instruction rip-offset)
  (cons (first instruction)
	(mapcar (lambda (o)
		  (maybe-fix-ea-operand o rip-offset)) (rest instruction))))

(defun instruction-size (instruction)
  (let ((f (first instruction)))
    (cond ((eq f :jump-fixup)
	   (jump-instruction-size instruction))
	  ((eq f :label)
	   0)
	  ((numberp f)
	   (length instruction))
	  (t (length (assemble-instruction
		      (maybe-fix-unresolved-offset-instruction instruction)))))))

(defun code-size (instructions)
  (let ((size 0))
    (dolist (inst instructions)
      (setf size
	    (+ size (instruction-size inst))))
    size))

(defun assembly-pass-1 (instructions)
  (let (code)
    (dolist (inst instructions)
      (let ((mnemonic (first inst)))
	(cond ((instruction-has-unresolved-memory-operand inst)
	       (push inst code))
	      ((eq mnemonic :label))
	      (t (push (assemble-instruction inst) code)))))
    (reverse code)))

(defun resolve-assembly-jumps (instructions)
  (set-jump-offsets (set-jump-offsets instructions :normal) :reverse))
