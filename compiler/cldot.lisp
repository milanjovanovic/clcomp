(defpackage #:clcomp.cldot
  (:use :cl))

(in-package :clcomp.cldot)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("cl-who" "cl-dot"))
  (declaim (optimize (debug 3) (safety 3) (speed 0))))


(setf cl-dot::*dot-path* "/opt/homebrew/bin/dot")


(defmethod cl-dot:graph-object-node ((graph clcomp.ssa::lambda-ssa) (object clcomp.ssa::ssa-block))
  (make-instance 'cl-dot:node
                 :attributes `(:label ,(format nil "~A" (clcomp.ssa::ssa-block-index object))
                               :shape :box)))

(defun get-successors-with-tags (block lambda-ssa)
  (let ((connections (list
		      (cons :order (let ((next (clcomp.ssa::lambda-ssa-get-next-order-block lambda-ssa block)))
				     (and next (clcomp.ssa::ssa-block-index next))))
		      (cons :succ (clcomp.ssa::ssa-block-succ block))
		      (cons :uncond-jump (clcomp.ssa::ssa-block-uncond-jump block))
		      (cons :cond-jump (clcomp.ssa::ssa-block-cond-jump block)))))
    (loop for (tag . bi) in connections
	  if bi collect (cons tag bi))))

(defun format-ssa-place (place)
  (etypecase place
    (clcomp.ssa::named-place
     (format nil "~A" (clcomp.ssa::get-place-name place)))
    (clcomp.ssa::rcv-argument-place (format nil "RCV-ARG-PLACE-~A" (clcomp.ssa::rcv-argument-place-index place)) )
    (clcomp.ssa::arg-place (format nil "ARG-REG-~A" (clcomp.ssa::argument-place-index place)))
    (clcomp.ssa::argument-count-place "ARG-COUNT-REG" )
    (clcomp.ssa::immediate-constant (format nil "CONST ~A" (clcomp.ssa::immediate-constant-constant place)))
    (clcomp.ssa::function-value-place "FUNCTION-REG")
    (clcomp.ssa::return-value-place (format nil "FUN-RETURN-~A" (clcomp.ssa::return-value-place-index place)))))

(defun format-ir (ir)
  (format nil "~A: ~A" (clcomp.ssa::ssa-form-index ir)
	  (etypecase ir
	    (clcomp.ssa::lambda-entry "entry")
	    (clcomp.ssa::arg-check "arg-check")
	    (clcomp.ssa::ssa-load (format nil "load ~A, ~A"
					  (format-ssa-place  (clcomp.ssa::ssa-load-to ir))
					  (format-ssa-place (clcomp.ssa::ssa-load-from ir))))
	    (clcomp.ssa::ssa-return "return")
	    (clcomp.ssa::ssa-multiple-return (format nil "return-multiple ~A" (clcomp.ssa::ssa-multiple-return-count ir)))
	    (clcomp.ssa::ssa-vop "FIXME: SSA-VOP")
	    (clcomp.ssa::ssa-unknown-values-fun-call (format nil "funcall-unknown ~A" (clcomp.ssa::ssa-unknown-values-fun-call-fun ir)))
	    (clcomp.ssa::ssa-known-values-fun-call (format nil "funcall-known ~A" (clcomp.ssa::ssa-known-values-fun-call-fun ir)))
    
	    (clcomp.ssa::ssa-go (format nil "go ~A" (clcomp.ssa::ssa-go-label ir)))
	    (clcomp.ssa::ssa-label (format nil "label ~A" (clcomp.ssa::ssa-label-label ir)))
	    (clcomp.ssa::ssa-if (format nil "if ~A GO ~A else ~A" (format-ssa-place (clcomp.ssa::ssa-if-test ir))
					;; (ssa-if-true-block ir)
					(clcomp.ssa::ssa-if-true-block-label ir)
					(clcomp.ssa::ssa-if-false-block-label ir)))
	    (clcomp.ssa::ssa-mvb-bind (format nil "ssa-mvb-bind: ~A" (clcomp.ssa::ssa-mvb-bind-places ir)))
	    (clcomp.ssa::ssa-value "FIXME: SSA-VALUE"))))

(defun format-phi (phi)
  (format nil "~A: ~{~a~^, ~}" (car phi)
	  (etypecase (cdr phi)
	    (clcomp.ssa::phi 
	     (mapcar #'clcomp.ssa::get-place-name (clcomp.ssa::phi-operands (cdr phi))))
	    (clcomp.ssa::virtual-place
	     (list (clcomp.ssa::get-place-name (cdr phi)))))))

(defun format-defined-place (dplace)
  (format nil "~A : ~A" (car dplace) (clcomp.ssa::get-place-name (cdr dplace))))
(defun format-block (block)
  (let ((dplaces (clcomp.ssa::ssa-block-defined block))
	(ir (clcomp.ssa::ssa-block-ssa block)))
    (format nil "<~A>"
	    (cl-who:with-html-output-to-string (s)
	      (:table :border "0"  :cellborder "1" :cellspacing "0" :cellpadding "1"
		      (:tr (:td (:b (cl-who:str (format nil "block: ~A"(clcomp.ssa::ssa-block-index block))))))
		      (when (clcomp.ssa::ssa-block-label block)
			(cl-who:htm
			 (:tr (:td (:b (cl-who:str (cl-who:escape-string (format nil "label ~A"
										 (clcomp.ssa::ssa-block-label block)))))))))
		      (when (clcomp.ssa::ssa-block-is-header block)
			(cl-who:htm
			 (:tr (:td (:b (cl-who:str (cl-who:escape-string (format nil "HEADER ~A"
										 (clcomp.ssa::ssa-block-is-header block)))))))))
		      (:tr (:td 
			    (loop for place in dplaces
				  do (cl-who:htm (cl-who:str (cl-who:escape-string (format-defined-place place))) (:br)))))
		      (:tr (:td 
			    (loop for phi in (clcomp.ssa::ssa-block-phis block)
				  when (clcomp.ssa::phi-p (cdr phi))
				    do (cl-who:htm (cl-who:str (cl-who:escape-string (format-phi phi))) (:br)))))
		      (:tr (:td :align "left"
				(loop for ins in ir
				      do (cl-who:htm (cl-who:str (cl-who:escape-string (format-ir ins))) (:br :align "left"))))))))))

(defun format-bl (sblock)
  (format nil "~A" (clcomp.ssa::ssa-block-index sblock)))


(defmethod cl-dot:graph-object-points-to ((graph clcomp.ssa::lambda-ssa) (block clcomp.ssa::ssa-block))
  (mapcar (lambda (s)
	    (let* ((tag (car s))
		   (succ-block-index (cdr s))
		   (sblock (clcomp.ssa::ssa-find-block-by-index graph succ-block-index))
		   (color (ecase tag
			    (:order "white")
			    (:succ "green")
			    (:uncond-jump "blue")
			    (:cond-jump "red"))))	      
	      (make-instance 'cl-dot:attributed
			     :object sblock
			     :attributes `(:weight 1 :color ,color))))
	  (get-successors-with-tags block graph)))


(defmethod cl-dot:graph-object-edges ((graph clcomp.ssa::lambda-ssa))
  (loop for l on (clcomp.ssa::lambda-ssa-blocks graph)
	if (cdr l)
	  collect (list  (car l) (cadr l) '(:color "gray" :arrowsize 0.1 :style :dotted))))


(defmethod cl-dot:graph-object-node ((graph clcomp.ssa::lambda-ssa) (sblock clcomp.ssa::ssa-block))
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
						  (clcomp.ssa::lambda-ssa-blocks lambda-ssa)
						  '(:outputorder :nodesfirst))))
    (cl-dot:dot-graph dgraph (format nil "/Users/milan/projects/clcomp.github/ssa_graphs/~A.png" file) :format :png)))

(setf clcomp.ssa::*generate-graph-fun* #'generate-graph)


(defclass no-order-graph ()
  ((data :accessor graph-data :initarg :data)))

(defclass straight-graph (no-order-graph)
  ())



