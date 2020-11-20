(defun list (&rest rest)
  rest)

(defun null (thing)
  (declare (inline null))
  (null thing))

(defun consp (thing)
  (declare (inline null listp))
  (if (null thing)
      nil
      (listp thing)))

(defun atom (thing)
  (not (consp thing)))

(defun cons (car cdr)
  (declare (inline cons))
  (cons car cdr))

(defun car (cons)
  (declare (inline listp car))
  (if (listp cons)
      (car cons)
      (error "CAR: Arguments is not of type LIST")))

(defun cdr (cons)
  (declare (inline cdr))
  (if (listp cons)
      (cdr cons)
      (error "CDR: Arguments is not of type LIST")))

(defun caar (cons)
  (car (car cons)))

(defun cddr (cons)
  (cdr (cdr cons)))

(defun cadr (cons)
  (car (cdr cons)))

(defun caddr (cons)
  (car (cdr (cdr cons))))

(defun cadddr (cons)
  (car (cdr (cdr (cdr cons)))))

(defun rplaca (cons e)
  (declare (inline listp rplaca))
  (if (listp cons)
      (rplaca cons e)
      (error "Argument is not of type LIST")))

(defun rplacd (cons e)
  (declare (inline listp rplacd))
  (if (listp cons)
      (rplacd cons e )
      (error "Argument is not of type LIST")))

(defun listp (thing)
  (declare (inline listp))
  (listp thing))

(defun first (list)
  (declare (inline car))
  (car list))

(defun second (list)
  (cadr list))

(defun third (list)
  (caddr list))

(defun fourth (list)
  (cadddr list))

(defun %cons-sxhash-depth (cons hash depth)
  (if (= depth 0)
      0
      (+ (sxhash (car cons))
	 (%cons-sxhash-depth (cdr cons) hash (- depth 1)))))

(defun %sxhash-cons (cons)
  (abs (%cons-sxhash-depth cons 17 8)))

(defun list-length (list)
  (if (listp list)
      (let ((x 0))
	(dolist (l list)
	  (setf x (+ x 1)))
	x)
      (error "Argument is not of type List")))

(defun list-reverse (list)
  (if (listp list)
      (let ((current nil))
	(dolist (l list)
	  (setf current (cons l current)))
	current)
      (error "Argument is not of type List")))

;;; FIXME. list can be improper
(defun copy-list (list)
  (let ((new-list nil))
    (dolist (elem list))))

(defun append (&rest lists)
  (if (null lists)
      nil
      (if (= 1 (list-length lists))
	  (first lists)
	  (let* ((result-list nil)
		 (current-result-cdr nil))
	    (do* ((lcdr lists (cdr lcdr))
		  (lcar (car lcdr) (car lcdr)))
		 ((null (cdr lcdr))
		  (setf (cdr current-result-cdr) lcar)
		  result-list)
	      (dolist (elem lcar)
		(let ((new-cdr (cons elem nil)))
		  (if (null result-list)
		      (progn
			(setf result-list new-cdr)
			(setf current-result-cdr new-cdr))
		      (progn
			(setf (cdr current-result-cdr) new-cdr)
			(setf current-result-cdr new-cdr))))))))))

(defun list* (&rest rest)
  (do ((cdr rest (cdr cdr)))
       ((null (cddr cdr))
	(setf (cdr cdr) (cadr cdr))
	rest)))

(defun nthcdr (n list)
  (do ((l list (cdr l))
       (index 0 (+ 1 index)))
      ((or (null l)
	   (= index n))
       l)))

(defun nth (n list)
  (car (nthcdr n list)))

(defun getf (place indicator)
  (do ((place place (cddr place)))
      ((or (eq (car place) indicator)
	   (null place))
       (cadr place))))

(defun cars-and-cdrs (lists)
  (let ((cars nil)
	(cdrs nil))
    (dolist (list lists)
      (let ((f (car list))
	    (r (cdr list)))
	(if f
	    (progn
	      (setf cars (cons f cars))
	      (setf cdrs (cons r cdrs)))
	    (return-from cars-and-cdrs nil))))
    (list (list-reverse cars)
	  (list-reverse cdrs))))

(defun mapcar (function list &rest more-lists)
  (let ((lists (cons list more-lists))
	(ccar nil)
	(ccdr nil))
    (do* ((pairs (cars-and-cdrs lists) (cars-and-cdrs (second pairs)))
	  (cars (car pairs) (car pairs)))
	 ((null (car pairs)) nil)
      (let ((result-cons (cons (apply function cars) nil)))
	(if (null ccar)
	    (progn
	      (setf ccar result-cons)
	      (setf ccdr result-cons))
	    (progn
	      (setf (cdr ccdr) result-cons)
	      (setf ccdr result-cons)))))
    ccar))
