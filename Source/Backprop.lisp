(in-package :ML)

;;;**************************************************************************
;;;
;;; FILE CONTENTS:
;;; -------------
;;;
;;; ARTIFICIAL NEURAL NETWORKS
;;;
;;; The BackPropagation implemented  here is based on the algorithm described
;;; in Table 4.2 of Chpt. 4 of "Machine "Learning" by Tom Mitchell.
;;; ISBN 0-07-042807-7.
;;;
;;;***************************************************************************

;;;***************************************************************************
;;;
;;; PART I:  ARTIFICIAL NEURAL NETWORKS & BACKPROP
;;; PART II: BINARY ENCODING OF STRINGS
;;;
;;;***************************************************************************

;;;***************************************************************************
;;;
;;; THE BACKPROPAGATION ALGORITHM
;;;
;;; BACKPROPAGATION (training-examples, nu, n(in), n(out), n(hidden))
;;;
;;;    Each training example is a pair of the form (x,t) where x is the vector
;;;    of network input values and t is the vector of network output values.
;;;
;;;    nu is the learning rate. n(in) is the number of input units, n(out) is
;;;    the number of output units and n(hidden) is the number of hidden units.
;;;
;;;    The input from unit i to unit j is denoted x(j,i) and the weight from
;;;    unit i to unit j is denoted w(j,i).
;;;
;;;    A) Create a feed-forward netword with n(in), n(out) & n(hidden) units.
;;;
;;;    B) Initialize all network weights to small random number between -0.5
;;;       and 0.5
;;;
;;;    3) Repeat until the termination condition is met:
;;;
;;;       For each (x,t) in the training examples do:
;;;
;;;           (Propagate the input forward through the network)
;;;
;;;           1. Input the instance x to the network and compute the output
;;;              o(u) for each unit u in the network.
;;;
;;;           (Propagate the errors backward through the network)
;;;
;;;           2. For each network output unit k, calculate its error delta(k)
;;;
;;;              delta(k) = o(k)(1 - o(k))(t(k) - o(k))
;;;
;;;           3. For each hidden unit h, calculate it's error term delta(h)
;;;
;;;              delta(h) = o(h) * (1 - o(h)) *    SUM   w(k,h) * delta(k)
;;;                                               k in
;;;                                              outputs
;;;
;;;           4. Update each network weight w(j,i)
;;;
;;;              w(j,i) = w(j,i) *  DELATA (w(j,i))  where,
;;;
;;;              DELTA(w(j,i)) = nu * delta(j) * x(j,i)
;;;
;;;
;;; Note: This the stochastic gradient descent version of the Backpropagation
;;; algorithm for feedforward networks containing two layers of signmoid units.
;;;
;;;***************************************************************************


;;;---------------------------------------------------------------------------
;;; The Learning Rate
;;;---------------------------------------------------------------------------

;;;These values work well with the identity function example.

(defparameter *ann-learning-rate* 0.3)
(defparameter *min-initial-weight* -0.1)
(defparameter *max-initial-weight* 0.1)

;;; Chapt. 4 of ML recommend rate of 0.05 and a range of (-0.5, 0.5) for
;;; the initial weights.

;;;---------------------------------------------------------------------------
;;; SIGMOID UNIT CLASSES
;;;---------------------------------------------------------------------------

(defclass SIGMOID-UNIT ()
  ((size :initarg :size
	 :initform 1
	 :accessor su-size)
   (inputs :initarg :inputs
	   :initform nil
	   :accessor su-inputs)
   (weights :initarg :weights
	    :initform nil
	    :accessor su-weights)
   (output :initform 0
	   :accessor su-output)))

;;;---------------------------------------------------------------------------

(defmethod INITIALIZE-INSTANCE :after ((obj SIGMOID-UNIT) &rest rest)
  (let* ((size (su-size obj))
	 (inputs (getf rest :inputs)))
    ;; Create the inputs of the sigmoid unit intialized to 0
    (setf (su-inputs obj)
      (if inputs
	  (make-array `(,size) :initial-contents inputs)
	(make-array `(,size) :initial-element 0)))
    ;; Create the weights of the sigmoid unit initialized small value
    (setf (su-weights obj)
      (make-array `(,size) :initial-element 0))
    ;; Initialize the weights
    (dotimes (i size)
      (setf (aref (su-weights obj) i)
	(initial-weight-value)))))

;;;---------------------------------------------------------------------------

(defun INITIAL-WEIGHT-VALUE ()
  (let* ((range (- *max-initial-weight* *min-initial-weight*))
	 (value (* 100 (random (1+ range)))))
    (float (- (/ value 100) (/ range 2)))))

;;;---------------------------------------------------------------------------

(defun MAKE-SU-UNIT (size &key (inputs nil)(class 'sigmoid-unit))
  (make-instance class :size size :inputs inputs))

;;;---------------------------------------------------------------------------
;;; INPUT-UNIT, HIDDEN-UNIT & OUTPUT-UNIT
;;;---------------------------------------------------------------------------

(defclass HIDDEN-UNIT (sigmoid-unit)
  ((error :initarg :error
	  :initform 0
	  :accessor su-error)))

;;;---------------------------------------------------------------------------

(defclass OUTPUT-UNIT (sigmoid-unit)
  ((error :initarg :error
	  :initform 0
	  :accessor su-error)))

;;;---------------------------------------------------------------------------
;;; SU-NTH-INPUT-VALUE
;;;---------------------------------------------------------------------------

(defmethod SU-NTH-INPUT-VALUE (n (su SIGMOID-UNIT))
  (aref (su-inputs su) n))

;;;---------------------------------------------------------------------------
;;; COMPUTE-SU-OUTPUT
;;;---------------------------------------------------------------------------

 (defmethod COMPUTE-SU-OUTPUT ((su SIGMOID-UNIT))
  (let* ((size (su-size su))
	 (weights (su-weights su))
	 (net 1))
    (dotimes (i size)
      (incf net (* (su-nth-input-value i su)(aref weights i))))
    (setf (su-output su) (/ 1 (+ 1 (exp (- net)))))))

;;;---------------------------------------------------------------------------
;;; ARTIFICIAL-NEURAL-NETWORK CLASS
;;;---------------------------------------------------------------------------

(defclass ARTIFICIAL-NEURAL-NETWORK ()
  ((inputs :initarg :inputs
	   :initform 0
	   :accessor ann-inputs)
   (hidden-units :initarg :hidden-units
		 :initform nil
		 :accessor ann-hidden-units)
   (output-units :initarg :output-units
		 :initform nil
		 :accessor ann-output-units)
   (learning-rate :initarg :learning-rate
		  :initform *ann-learning-rate*
		  :accessor ann-learning-rate)
   (training-data :initarg :training-data
		  :initform nil
		  :accessor ann-training-data)))

;;;---------------------------------------------------------------------------
;;; MAKE-ANN
;;;---------------------------------------------------------------------------

(defun MAKE-ANN (inputs hidden outputs &optional (training-data nil))
  (let* ((hidden-units-array (make-array `(,hidden)))
	 (output-units-array (make-array `(,outputs))))
    (dotimes (i hidden)
      (setf (aref hidden-units-array i)
	(make-su-unit inputs :class 'hidden-unit)))
    (dotimes (i outputs)
      (setf (aref output-units-array i)
	(make-su-unit hidden :class 'output-unit
			  #+IGNORE :inputs #+IGNORE hidden-units)))
    (make-instance 'ARTIFICIAL-NEURAL-NETWORK
      :inputs inputs
      :hidden-units hidden-units-array
      :output-units output-units-array
      :training-data training-data)))

;;---------------------------------------------------------------------------
;;; BACKPROP
;;;---------------------------------------------------------------------------

(defun BACKPROP (inputs hidden outputs
		 &key (training-data nil)
		      (iterations 20)
		      (ann (make-ann inputs hidden outputs training-data)))
  (time
  (dotimes (i iterations)
    ;;(format t "~%Iteration: ~a" i)
    (dolist (training-instance training-data)
      (let ((td-x (first training-instance))
	    (td-t (second training-instance)))
	;; 1. Input td-x to the network and compute all outputs
	(compute-ann-output-for-input-value ann td-x)

	;; 2. Compute the output unit error for each output unit
	(dotimes (i outputs)
	  (let* ((output-unit (aref (ann-output-units ann) i))
		 (output-value (su-output output-unit)))
	    (setf (su-error output-unit)
	      (* output-value
		 (- 1 output-value)
		 (- (aref td-t i) output-value)))))

	;; 3. Compute the hidden unit error for each hidden unit
	(dotimes (i hidden)
	  (let* ((hidden-unit (aref (ann-hidden-units ann) i))
		 (output-value (su-output hidden-unit))
		 (sum 0))
	    (dotimes (j outputs)
	      (let ((output-unit (aref (ann-output-units ann) j)))
		(incf sum (* (aref (su-weights output-unit) i)
			     (su-error output-unit)))))
	    (setf (su-error hidden-unit)
	      (* output-value (- 1 output-value) sum))))

	;; 4. Update network weights
	(dotimes (j hidden)
	  (let* ((hidden-unit (aref (ann-hidden-units ann) j)))
	    (dotimes (i (su-size hidden-unit))
	      (setf (aref (su-weights hidden-unit) i)
		(+  (aref (su-weights hidden-unit) i)
		    ;; DELTA(w(j,i))
		    (*  *ann-learning-rate*
			(su-error hidden-unit)
			(aref (su-inputs hidden-unit) i)))))))
	(dotimes (j outputs)
	  (let* ((output-unit (aref (ann-output-units ann) j)))
	    (dotimes (i (su-size output-unit))
	      (setf (aref (su-weights output-unit) i)
		(+  (aref (su-weights output-unit) i)
		    ;; DELTA(w(j,i))
		    (*  *ann-learning-rate*
			(su-error output-unit)
			(aref (su-inputs output-unit) i)))))))))))

  ;; (print-ann-output-error ann)
  ;; (print-ann-output-value ann)
  ann)


;;;---------------------------------------------------------------------------
;;; COMPUTE-ANN-OUTPUT-FOR-INPUT-VALUE
;;;---------------------------------------------------------------------------

(defun COMPUTE-ANN-OUTPUT-FOR-INPUT-VALUE (ann input-vector)
  ;; Set the inputs and compute the outputs of the hidden units
  (let* ((hidden-units (ann-hidden-units ann))
	 (output-units (ann-output-units ann)))
    (dotimes (i (length hidden-units))
      (setf (su-inputs (aref hidden-units i)) input-vector)
	(compute-su-output (aref hidden-units i)))
    ;; Set the inputs and compute the outputs of the output-units
    (dotimes (i (length output-units))
      (let* ((output-unit (aref output-units i)))
	(dotimes (j (length hidden-units))
	  (setf (aref (su-inputs output-unit) j)
	    (su-output (aref hidden-units j))))
	(compute-su-output output-unit)))))

;;;---------------------------------------------------------------------------
;;; ANN-OUTPUT-VALUES
;;;---------------------------------------------------------------------------

(defun ANN-OUTPUT-VALUES (ann)
  (let* ((output-units-array (ann-output-units ann))
	 (output-values nil))
    (dotimes (i (length output-units-array))
      (push (round (su-output (aref output-units-array i)))
	    output-values))
    (reverse output-values)))

;;;---------------------------------------------------------------------------
;;;  ANN-ERROR-VALUES
;;;---------------------------------------------------------------------------

(defun ANN-ERROR-VALUES (ann)
  (let* ((error-units-array (ann-output-units ann))
	 (error-values nil))
    (dotimes (i (length error-units-array))
      (push (round (su-error (aref error-units-array i)))
	    error-values))
    (reverse error-values)))

;;;---------------------------------------------------------------------------
;;; MISC FUNCTIONS
;;;---------------------------------------------------------------------------

(defun PRINT-ANN-OUTPUT-VALUE (ann)
  (format t "~%ANN Output: ")
  (dolist (output (ann-output-values ann))
    (format t "~6,6F  " output)))

;;;---------------------------------------------------------------------------

(defun PRINT-ANN-OUTPUT-ERROR (ann)
  (format t "~%ANN Error: ")
  (dolist (error (ann-error-values ann))
    (format t "~6,6F  " error)))

;;;***************************************************************************
;;;
;;; PART II: BINARY ENCODING OF STRINGS & BINARY STRING BP
;;;
;;;***************************************************************************

;;;---------------------------------------------------------------------------
;;; STRING --> BINARY-STRING
;;;---------------------------------------------------------------------------

(defun CONVERT-CHARACTER-TO-BINARY-STRING (number)
  (let* ((string (make-string 8 :initial-element #\0))
	 (bstring (format nil "~B" (char-code number)))
	 (size (length bstring)))
    (dotimes (i size)
      (setf (aref string (- (1- (length string)) i))
	(aref bstring (- (1- (length bstring)) i))))
    string))

;;;---------------------------------------------------------------------------

(defun CONVERT-STRING-TO-BINARY-ARRAY (string)
  (let* ((binary-strings nil)
	 (binary-string nil)
	 (binary-array nil))
    (dotimes (i (length string))
      (push (convert-character-to-binary-string (aref string i))
	    binary-strings))
    (setf binary-string
      (apply #'concatenate 'STRING (reverse binary-strings)))
    (setf binary-array
      (make-array `(,(length binary-string)) :initial-element 0))
    (dotimes (i (length binary-string))
      (setf (aref binary-array i)
	(if (char= (aref binary-string i) #\0) 0 1)))
    binary-array))

;;;---------------------------------------------------------------------------

(defun CONVERT-STRING-PAIR-TO-BINARY-ARRAY-PAIR (string-pair)
  (setf (first string-pair)
    (convert-string-to-binary-array (first string-pair)))
  (setf (second string-pair)
    (convert-string-to-binary-array (second string-pair))))

;;;---------------------------------------------------------------------------

(defun CONVERT-STRING-PAIRS-TO-BINARY-ARRAY-PAIRS (string-pairs)
  (dolist (string-pair string-pairs)
    (convert-string-pair-to-binary-array-pair string-pair))
  string-pairs)

;;;---------------------------------------------------------------------------
;;; BINARY-STRING ---> STRING
;;;---------------------------------------------------------------------------

(defun CONVERT-BINARY-STRING-TO-STRING (binary-string)
  (let* ((string-length (round (/ (length binary-string) 8)))
	 (string (make-string string-length :initial-element #\space)))
    (dotimes (i string-length)
      (setf (aref string i)
	(code-char (convert-string-byte-to-decimal binary-string (* i 8)))))
    string))

;;;---------------------------------------------------------------------------

(defun CONVERT-STRING-BYTE-TO-DECIMAL (string-byte &optional (offset 0))
  (let* ((number 0))
    (dotimes (i 8)
      (when (= (aref string-byte (+ offset (- 7 i))) 1)
	(incf number (expt 2 i))))
    number))

;;;---------------------------------------------------------------------------
;;; MAKE-STRING-PAIRS-ANN
;;;---------------------------------------------------------------------------

(defun MAKE-BINARY-ARRAY-PAIRS-ANN (binary-array-pairs)
  (let* ((input-size (length (first (first binary-array-pairs))))
	 (hidden (round (sqrt input-size)))
	 (output-size (length (second (first binary-array-pairs)))))
    (make-ann input-size hidden output-size binary-array-pairs)))

;;;---------------------------------------------------------------------------
;;; IDENTITY FUNCTION EXAMPLE
;;;---------------------------------------------------------------------------

(defvar *IDENTITY-FUNCTION-EXAMPLE*
    `((,(make-array '(8) :initial-contents '(1 0 0 0 0 0 0 0))
       ,(make-array '(8) :initial-contents '(1 0 0 0 0 0 0 0)))
      (,(make-array '(8) :initial-contents '(0 1 0 0 0 0 0 0))
       ,(make-array '(8) :initial-contents '(0 1 0 0 0 0 0 0)))
      (,(make-array '(8) :initial-contents '(0 0 1 0 0 0 0 0))
       ,(make-array '(8) :initial-contents '(0 0 1 0 0 0 0 0)))
      (,(make-array '(8) :initial-contents '(0 0 0 1 0 0 0 0))
       ,(make-array '(8) :initial-contents '(0 0 0 1 0 0 0 0)))
      (,(make-array '(8) :initial-contents '(0 0 0 0 1 0 0 0))
       ,(make-array '(8) :initial-contents '(0 0 0 0 1 0 0 0)))
      (,(make-array '(8) :initial-contents '(0 0 0 0 0 1 0 0))
       ,(make-array '(8) :initial-contents '(0 0 0 0 0 1 0 0)))
      (,(make-array '(8) :initial-contents '(0 0 0 0 0 0 1 0))
       ,(make-array '(8) :initial-contents '(0 0 0 0 0 0 1 0)))
      (,(make-array '(8) :initial-contents '(0 0 0 0 0 0 0 1))
       ,(make-array '(8) :initial-contents '(0 0 0 0 0 0 0 1)))))

(defvar *ife* *IDENTITY-FUNCTION-EXAMPLE*)

;;;---------------------------------------------------------------------------

(defun TRAIN-ANN-FOR-IFE ()
  (format t "~%Training ANN for IDENTITY FUNCTION example...~%")
  (let* ((ann (backprop 8 3 8
			:training-data *IDENTITY-FUNCTION-EXAMPLE*
			:iterations 50000)))
    (format t "~%Verifying resulting ANN:~%")
    (dolist (instance *IDENTITY-FUNCTION-EXAMPLE*)
      (format t "~%ANN Input Value:  ~a" (first instance))
      (compute-ann-output-for-input-value ann (first instance))
      (format t "~%ANN Output Value: ~a" (ann-output-values ann))
      (format t "~%Correct Output Value:  ~a~%" (second instance)))
    ann))

;;;---------------------------------------------------------------------------
;;; End of File
;;;---------------------------------------------------------------------------
