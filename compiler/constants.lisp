(in-package #:clcomp)

(defparameter *allocation-size* 8)
(defparameter *word-size* 8)

(defparameter *tag-size* 3)
(defparameter *mask* 7)

(defparameter *fixnum-tag* 0)
(defparameter *list-tag* 2)
(defparameter *function-tag* 3)
(defparameter *char-tag* 4)
(defparameter *pointer-tag* 7)


(defparameter *simple-array-tag* 1)

;;; tag 5 and 6 are free

(defparameter *nil* 536870914)
(defparameter *t* 536870927)

(defparameter *most-positive-fixnum* (- (expt 2 (- (* *word-size* 8)
						   (+ 1 *tag-size*))) 1))

(defparameter *most-negative-fixnum* (- (expt 2 (- (* *word-size* 8)
						   (+ 1 *tag-size*)))))


(defparameter *array-header-size* 3) ;; look at lispo.h

(defun fixnumize (num)
  (if (and (> num *most-negative-fixnum*)
	   (< num *most-positive-fixnum* ))
      (ash num *tag-size*)
      (error "Number is to big to be fixnum !!!")))
