(in-package #:clcomp)

(defparameter *allocation-size* 8)
(defparameter *word-size* 8)

(defparameter *tag-size* 3)
(defparameter *mask* 7)

(defparameter *fixnum-tag* 0)
(defparameter *pointer-tag* 1)
(defparameter *list-tag* 2)
(defparameter *function-tag* 3)
(defparameter *char-tag* 4)
(defparameter *symbol-tag* 5)
(defparameter *single-float-tag* 6)

(defparameter *exteneded-tag-size* 8)
(defparameter *extended-tag-mask* 255)


(defparameter *extended-tags*
  '((simple-array 209)
    (string 217)))

(defparameter *largest-extended-tag* 249)

(defun get-extended-tag (what)
  (second (assoc what *extended-tags*)))

;;; tag 1 is free ??

(defparameter *nil* 536870914)
(defparameter *t* 536870927)

(defparameter *most-positive-fixnum* (- (expt 2 (- (* *word-size* 8)
						   (+ 1 *tag-size*))) 1))

(defparameter *most-negative-fixnum* (- (expt 2 (- (* *word-size* 8)
						   (+ 1 *tag-size*)))))


(defparameter *array-header-size* 4) ;; look at lispo.h

(defun fixnumize (num)
  (if (and (> num *most-negative-fixnum*)
	   (< num *most-positive-fixnum* ))
      (ash num *tag-size*)
      (error "Number is to big to be fixnum !!!")))

(defun characterize (char)
  (let ((code (char-code char)))
    (+ (ash code *tag-size*) *char-tag*)))
