(in-package :Machine-Learning)


(defun COST-FUNCTION-2 (t0 t1 hfn xv yv)
   (let ((2m (* 1.0 2 (length xv))))
     (/ (apply #'+ (mapcar #'(lambda (x y)
			       (expt (- (funcall hfn t0 t1 x) y) 2))
			   xv
			   yv))
	2m)))

(defun LINEAR (t0 t1 x)
  (+ t0 (* t1 x)))

(defmethod mm ((m1 LIST)(m2 LIST))
  (let ((d1 (length (first m1)))
	(d2 (length m2))
	(m2-transpose (transpose m2)))
    (unless (= d1 d2)
      (error "Dimension are not congruent for multiplication."))
    (mapcar #'(lambda (m1-row)
		(mapcar #'(lambda (m2-col)
			    (apply #'+
				   (mapcar #'(lambda (m1-e m2-e)
					       (* m1-e m2-e))
					   m1-row
					   m2-col)))
			m2-transpose))
	    m1)))

(defun TRANSPOSE (m)
  (cond ((null (first m))
	 nil)
	(t
	 (cons (mapcar #'first m)
	       (transpose (mapcar #'rest m))))))

(defvar X '(89 72 94 69))

(defun NORMALIZE (value values)
  (let* ((min (apply #'min values))
	 (max (apply #'max values))
	 (range (- max min))
	 (mean (* 1.0 (/ (apply #'+ values)(length values)))))
    (/ (- value mean) range)))
  
(defvar m1 '((1 2 1 5)(0 3 0 4)(-1 -2 0 0)))
(defvar m2 '(1 3 2 1))

(defvar m3 '((1 0 3)(2 1 5)(3 1 2)))
(defvar m4  '(1 6 2))

(defvar m5 '((1 3 2)(4 0 1)))
(defvar m6 '((1 3)(0 1)(5 2)))