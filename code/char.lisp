(defun char-code (char)
  (declare (inline char-code))
  (char-code char))

(defun code-char (code)
  (declare (inline code-char))
  (code-char code))

(defun two-args-char= (c1 c2)
  (declare (inline eq))
  (eq c1 c2))

(defun char= (ch &rest rest)
  (dolist (c rest)
    (unless (char= ch c)
      (return-from char= nil)))
  t)

(defun char/= (ch &rest rest)
  (let ((rest-lenght (length rest)))
    (do ((index 0 (+ index 1))
	 (char ch (nth index rest)))
	((= index rest-lenght) t)
      (dotimes (ix (- rest-lenght index))
	(let ((next-index (+ index ix)))
	  (when (char= char (nth next-index rest))
	    (return-from char/= nil)))))))

(defun two-args-char-equal (c1 c2)
  (if (eq c1 c2)
      t
      (let ((cd1 (char-code c1))
	    (cd2 (char-code c2)))
	(and (= 32 (abs (- cd1 cd2)))
	     (or
	      (and (> cd1 96)
		   (< cd1 123))
	      (and (> cd2 96)
		   (< cd2 123)))))))

(defun char-equal (ch &rest rest)
  (dolist (c rest)
    (unless (char-equal ch c)
      (return-from char-equal nil)))
  t)

(defun char-not-equal (ch &rest rest)
  (let ((rest-lenght (length rest)))
    (do ((index 0 (+ index 1))
	 (char ch (nth index rest)))
	((= index rest-lenght) t)
      (dotimes (ix (- rest-lenght index))
	(let ((next-index (+ index ix)))
	  (when (char-equal char (nth next-index rest))
	    (return-from char-not-equal nil)))))))

(defun char-upcase (char)
  (let ((char-code (char-code char)))
    (if (and (> char-code 96)
	     (< char-code 123))
	(code-char (- char-code 32))
	char)))

(defun char-downcase (char)
  (let ((char-code (char-code char)))
    (if (and (> char-code 64)  
	     (< char-code 91))
	(code-char (+ char-code 32))
	char)))
