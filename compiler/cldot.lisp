(in-package :clcomp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (debug 3) (safety 3) (speed 0))))


(defmethod cl-dot:graph-object-node ((graph lambda-ssa) (object ssa-block))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(format nil "~A" (ssa-block-index object))
                               :shape :box)))

(defun get-successors-with-tags (block)
  (let ((connections (list
		      (cons :succ (ssa-block-succ block))
		      (cons :uncond-jump (ssa-block-uncond-jump block))
		      (cons :cond-jump (ssa-block-cond-jump block)))))
    (loop for (tag . bi) in connections
	  if bi collect (cons tag bi))))

(defun format-ssa-place (place)
  (etypecase place
    (named-place
     (format nil "~A" (named-place-name place)))
    (rcv-argument-place (format nil "RCV-ARG-PLACE-~A" (rcv-argument-place-index place)) )
    (arg-place (format nil "ARG-REG-~A" (argument-place-index place)))
    (argument-count-place "ARG-COUNT-REG" )
    (immediate-constant-node (format nil "CONST-NODE ~A" (immediate-constant-node-value place)))
    (immediate-constant (format nil "CONST ~A" (immediate-constant-constant place)))
    (function-value-place "FUNCTION-REG")
    (return-value-place (format nil "FUN-RETURN-~A" (return-value-place-index place)))))

(defun format-ir (ir)
  (format nil "~A: ~A" (ssa-form-index ir)
	  (etypecase ir
	    (lambda-entry "entry")
	    (arg-check "arg-check")
	    (ssa-load (format nil "load ~A, ~A"
			      (format-ssa-place  (ssa-load-to ir))
			      (format-ssa-place (ssa-load-from ir))))
	    (ssa-return "return")
	    (ssa-multiple-return "return-multiple")
	    (ssa-vop "FIXME: SSA-VOP")
	    (ssa-unknown-values-fun-call (format nil "funcall-unknown ~A" (ssa-unknown-values-fun-call-fun ir)))
	    (ssa-known-values-fun-call (format nil "funcall-known ~A" (ssa-known-values-fun-call-fun ir)))
    
	    (ssa-go (format nil "go ~A" (ssa-go-label ir)))
	    (ssa-label (format nil "label ~A" (ssa-label-label ir)))
	    (ssa-if (format nil "if ~A block ~A" (format-ssa-place (ssa-if-test ir))
			    (ssa-if-true-block ir)))
	    (ssa-value "FIXME: SSA-VALUE"))))

(defun format-phi (phi)
  (format nil "~A: ~{~a~^, ~}" (car phi) (etypecase (cdr phi)
					   (phi 
					    (mapcar #'named-place-name (phi-operands (cdr phi))))
					   (virtual-place
					    (list (named-place-name (cdr phi)))))))

(defun format-defined-place (dplace)
  (format nil "~A : ~A" (car dplace) (named-place-name (cdr dplace))))
(defun format-block (block)
  (let ((dplaces (ssa-block-defined block))
	(ir (ssa-block-ssa block)))
    (format nil "<~A>"
	    (cl-who:with-html-output-to-string (s)
	      (:table :border "0"  :cellborder "1" :cellspacing "0" :cellpadding "1"
		      (:tr (:td (:b (cl-who:str (format nil "block: ~A"(ssa-block-index block))))))
		      (when (ssa-block-label block)
			(cl-who:htm
			 (:tr (:td (:b (cl-who:str (cl-who:escape-string(format nil "label ~A" (ssa-block-label block)))))))))
		      (:tr (:td 
			    (loop for place in dplaces
				  do (cl-who:htm (cl-who:str (cl-who:escape-string(format-defined-place place))) (:br)))))
		      (:tr (:td 
			    (loop for phi in (ssa-block-phis block)
				  do (cl-who:htm (cl-who:str (cl-who:escape-string(format-phi phi))) (:br)))))
		      (:tr (:td :align "left"
				(loop for ins in ir
				      do (cl-who:htm (cl-who:str (cl-who:escape-string (format-ir ins))) (:br :align "left"))))))))))

(defun format-bl (sblock)
  (format nil "~A" (ssa-block-index sblock)))


(defmethod cl-dot:graph-object-points-to ((graph lambda-ssa) (block ssa-block))
  (mapcar (lambda (s)
	    (let* ((tag (car s))
		   (succ-block-index (cdr s))
		   (sblock (ssa-find-block-by-index graph succ-block-index))
		   (color (ecase tag
			    (:succ "green")
			    (:uncond-jump "blue")
			    (:cond-jump "red"))))	      
	      (make-instance 'cl-dot:attributed
			     :object sblock
			     :attributes `(:weight 1 :color ,color))))
	  (get-successors-with-tags block)))


(defmethod cl-dot:graph-object-edges ((graph lambda-ssa))
  (loop for l on (lambda-ssa-blocks graph)
	if (cdr l)
	  collect (list  (car l) (cadr l) '(:color "gray" :arrowsize 0.1 :style :dotted))))


(defmethod cl-dot:graph-object-node ((graph lambda-ssa) (sblock ssa-block))
  (make-instance 'cl-dot:node
                 :attributes `(:label (:htmls ,(format-block sblock))
                               :shape :plain
			       :style :filled
                               :color :black
			       :fillcolor "gray95"
			       :fontsize 12.0)))


;;; be sure to LOAD CL-DOT code from ~/projects/cl-dot because that version is fixed
(defun generate-graph (lambda-ssa file)
  (let ((dgraph (cl-dot:generate-graph-from-roots lambda-ssa
						  (lambda-ssa-blocks lambda-ssa)
						  '(:outputorder :nodesfirst))))
    (cl-dot:dot-graph dgraph (format nil "/Users/milan/projects/clcomp.github/ssa_graphs/~A.png" file) :format :png)))


(defclass no-order-graph ()
  ((data :accessor graph-data :initarg :data)))

(defclass straight-graph (no-order-graph)
  ())



