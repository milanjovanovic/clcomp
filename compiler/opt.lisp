(defpackage #:clcomp.ssa.opt
  (:use #:clcomp.ssa)
  (:use #:cl))

(in-package :clcomp.ssa.opt)

;; this is just simple LOAD VIRTUAl, VIRTUAL elimination so gdb tracing is easier to follow
(defun copy-prop-1 (lambda-ssa)
  (let ((loads (make-hash-table))
	(ir-indexes (make-hash-table)))
    (dolist (b (clcomp.ssa::lambda-ssa-blocks lambda-ssa))
      (dolist (ir (clcomp.ssa::ssa-block-ssa b))
	(typecase ir
	  (clcomp.ssa::ssa-load (let ((to (clcomp.ssa::ssa-load-to ir))
				      (from (clcomp.ssa::ssa-load-from ir))
				      (ir-index (clcomp.ssa::ssa-load-index ir)))
				  (when (and (clcomp.ssa::virtual-place-p to)
					     (clcomp.ssa::virtual-place-p from))
				    (setf (gethash to loads) from)
				    (setf (gethash to ir-indexes) ir-index))))
	  (otherwise t))))
    (values loads ir-indexes)))

(defun copy-prop-replace-defined (lambda-ssa loads)
  (let ((replacable (clcomp::hash-keys loads)))
    (dolist (b (clcomp.ssa::lambda-ssa-blocks lambda-ssa))
      (let* ((defined (clcomp.ssa::ssa-block-defined b))
	     (defined-places (mapcar #'cdr defined))
	     (olds (intersection replacable defined-places)))
	(when olds
	  ;; we want to delete all old references
	  (dolist  (old olds)
	    (setf defined (remove old defined :key #'cdr)))
	  (dolist (old olds)
	    (let ((new (gethash  old loads)))
	      (assert new)
	      (unless (position new defined :key #'cdr)
		(push (cons (clcomp.ssa::virtual-place-variable new) new) defined)))))
	(setf (clcomp.ssa::ssa-block-defined b) defined)))))

(defun copy-prop-replace-phi-operands (lambda-ssa loads)
  (declare (optimize debug))
  (let ((replacable (clcomp::hash-keys loads)))
    (dolist (phi (clcomp::hash-values (clcomp.ssa::lambda-ssa-all-phis lambda-ssa)))
      (when (and (clcomp.ssa::phi-p phi)
		 (not (clcomp.ssa::get-phi-place-reduced-value (clcomp.ssa::phi-place phi))))
	(let ((old-places (intersection replacable (clcomp.ssa::phi-operands phi))))
	  (when old-places
	    (dolist (old old-places)
	      (let ((index (position old (clcomp.ssa::phi-operands phi))))
		(setf (nth index (clcomp.ssa::phi-operands phi)) (gethash old loads))))))))
    ;; patch live-phi-operands
    (dolist (b (clcomp.ssa::lambda-ssa-blocks lambda-ssa))
      (let* ((live-phi-operands (clcomp.ssa::ssa-block-live-phi-operands b))
	     (places (mapcar #'cdr live-phi-operands))
	     (olds (intersection replacable places)))
	(when olds
	  (dolist (old olds)
	    ;; PLACES jave the same order as LIVE-PHI-OPERANDS
	    (let ((index (position old places)))
	      (setf (cdr (nth index live-phi-operands)) (gethash old loads)))))))))

(defun copy-prop-1-patch-lambda (lambda-ssa loads ir-indexes)
  (let ((replacable (clcomp::hash-keys loads))
	(remove-indexes (clcomp::hash-values ir-indexes)))
    (dolist (b (clcomp.ssa::lambda-ssa-blocks lambda-ssa))
      (let ((filtered-irs nil))
	(dolist (ir (clcomp.ssa::ssa-block-ssa b))
	  (when (find (clcomp.ssa::ssa-form-index ir) remove-indexes)
	    (print (list 'removing-ir ir)))
	  (unless (find (clcomp.ssa::ssa-form-index ir) remove-indexes)
	    (typecase ir
	      (clcomp.ssa::ssa-form-rw
	       (let ((reads-write (clcomp.ssa::ssa-form-rw-reads-writes ir)))
		 (let ((old-places (intersection replacable (first reads-write))))
		   (when old-places
		     (dolist (old old-places)
		       (clcomp.ssa::ssa-form-rw-replace-reads ir old (gethash old loads))))))))
	    (push ir filtered-irs)))
	(setf (clcomp.ssa::ssa-block-ssa b) (reverse filtered-irs))))))

(defun do-copy-prop-1 (lambda-ssa)
  (multiple-value-bind (loads ir-indexes) (copy-prop-1 lambda-ssa)
    (when loads
      (copy-prop-1-patch-lambda lambda-ssa loads ir-indexes)
      (copy-prop-replace-phi-operands lambda-ssa loads)
      ;; we don't actually need this, SSA-BLOCK-DEFINED is not used after SSA construction
      ;; (copy-prop-replace-defined lambda-ssa loads)
      )))


;; (defun add-copy (from to loads)
;;   (declare (optimize debug))
;;   (let ((chains (second loads)))
;;     (dolist (chain chains)
;;       (when (eq from (first (second chain)))
;; 	(push to (second chain))
;; 	(return-from add-copy)))
;;     (setf (second loads)
;; 	  (append (list (list nil (list to from)))
;; 		  (second loads)))))

;; (defun add-write (write loads)
;;   (print (list 'write write)))

;; (defun kill-chains (loads)
;;   (declare (optimize debug))
;;   (let ((chains (second loads)))
;;     (dolist (chain chains)
;;       (unless (eq 'kill (first (second chain)))
;; 	(push 'kill (second chain))))))

;; (defun copy-propagation (lambda-ssa)
;;   (let ((visited (make-hash-table))
;; 	(loads (list 'loads nil)))
;;     (dolist (b (lambda-ssa-blocks lambda-ssa))
;;       (unless (gethash (ssa-block-index b) visited)
;; 	(block-copy-propagation b visited loads lambda-ssa)))
;;     (break)
;;     loads))

;; (defun block-copy-propagation (b visited loads lambda-ssa)
;;   (declare (optimize debug))
;;   (setf (gethash (ssa-block-index b) visited) t)
;;   (dolist (ir (ssa-block-ssa b))
;;     (typecase ir
;;       (ssa-load (add-copy (ssa-load-from ir)
;; 			  (ssa-load-to ir)
;; 			  loads))
;;       (ssa-kill-form (kill-chains loads))
;;       (ssa-form-rw
;;        (let* ((rw (ssa-form-rw-reads-writes ir))
;; 	      (reads (first rw))
;; 	      (writes (second rw)))
;; 	 (declare (ignore reads))
;; 	 (dolist (w writes)
;; 	   (add-write w loads))))))
;;   (let ((successors-blocks (ssa-block-successors b lambda-ssa)))
;;     (if (< (length successors-blocks) 2)
;; 	(when (= 1 (length successors-blocks))
;; 	  (block-copy-propagation (first successors-blocks) visited loads lambda-ssa))
;; 	(progn
;; 	  (kill-chains loads)
;; 	  (dolist (bl successors-blocks)
;; 	    (unless (gethash (ssa-block-index b) visited)
;; 	      (block-copy-propagation bl visited loads lambda-ssa)))))))

