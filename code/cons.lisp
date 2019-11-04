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

(defun cons (car cdr)
  (declare (inline cons))
  (cons car cdr))

(defun car (cons)
  (declare (inline listp car))
  (if (listp cons)
      (car cons)
      (error "Arguments is not of type LIST")))

(defun cdr (cons)
  (declare (inline cdr))
  (if (listp cons)
      (cdr cons)
      (error "Arguments is not of type LIST")))

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

(defun list-length (list)
  (if (listp list)
      (let ((x 0))
	(dolist (l list)
	  (setf x (+ x 1)))
	x)
      (error "Argument is not of type List")))
