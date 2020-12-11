(in-package :ML)

;;;******************************************************************************
;;;
;;; FILE CONTENTS:
;;; -------------
;;;
;;; DECISION TREE LEARNING
;;;
;;; The Algorithm implemented here is based on the algorithm described in
;;; Chpt. 3 of "Machine Learning" by Tom Mitchell. ISBN 0-07-042807-7.
;;; This is the ID3 Algorithm originally developed by J.R. Quinlan
;;;
;;; Part I:   ENTROPY & GAIN
;;; ------------------------
;;;
;;; AVP-ATTRIBUTE
;;; AVP-VALUE
;;; FIND-ATTRIBUTE-VALUE
;;; ENTROPY
;;; GAIN
;;; HIGHEST-GAIN-ATTRIBUTE
;;; HIGHEST-ENTROPY-ATTRIBUTE
;;; LOWEST-ENTROPY-ATTRIBUTE
;;; COMPUTE-ATTRIBUTE-VALUES
;;; PARTITION-DATA-BY-ATTRIBUTE
;;; PRINT-DATA-PARTITIONS

;;; Part II:  The ID3 Algorithm
;;; ---------------------------
;;;
;;; ID3
;;; ALL-EXAMPLES-POSITIVE-P
;;; ALL-EXAMPLES-NEGATIVE-P
;;; ALL-ATTRIBUTES

;;;
;;;******************************************************************************


;;;------------------------------------------------------------------------------
;;; FIND-ATTRIBUTE-VALUE
;;;------------------------------------------------------------------------------

(defmethod FIND-ATTRIBUTE-VALUE ((attribute t)(avP-list LIST) &key (test #'equalp))
  (dolist (avP-entry avP-list)
    (when (funcall test attribute (avP-attribute avP-entry))
      (return (avP-value avP-entry)))))

;;;*****************************************************************************
;;;
;;; ENTRPOY & GAIN FUNCTIONS
;;; ------------------------
;;;
;;; If a target attribute can take on c different values, then the entropy of
;;; a collection S of examples relative to this c-wise classification is
;;; defined as:
;;;
;;;               c
;;; ENTROPY(S) = SUM  [- p(i) x log p(i)]
;;;              i=1
;;;
;;;
;;; The information gain, GAIN(S,A), of an attribute A, relative to a collection
;;; of examples S is defined by:
;;;
;;;                                      |S(v)|
;;; GAIN(S,A) = ENTROPY(S)   -   SUM     ------ x ENTROPY (S(v))
;;;                            Values(A)  |S|
;;;
;;;*****************************************************************************

;;;-----------------------------------------------------------------------------
;;; ENTROPY
;;;-----------------------------------------------------------------------------

;;; This computes the entropy of <target-attribute> from a list of
;;; sets of attribute-value pairs

(defmethod ENTROPY ((target-attribute SYMBOL)(tags-lists LIST))
  (let* ((target-values (compute-attribute-values target-attribute tags-lists))
	 (count 0)
	 (probabilities nil)
	 (size (length tags-lists))
	 (entropy 0))
    (dolist (target-value target-values)
      (setf count 0)
      (dolist (example tags-lists)
	(when (eq target-value (find-attribute-value target-attribute example))
	  (incf count)))
      (push (float (/ count size)) probabilities))
    (dolist (prob probabilities)
      (incf entropy (* (- prob) (log prob 2))))
    ;; Return the Entropy
    entropy))

;;;-----------------------------------------------------------------------------
;;; GAIN
;;;-----------------------------------------------------------------------------

(defmethod GAIN ((target-attribute SYMBOL)(attribute SYMBOL)(tags-lists LIST))
  (let* ((tags-lists-length (length tags-lists))
	 (avp-sets (partition-data-by-attribute attribute tags-lists))
	 (sum 0))
     (maphash #'(lambda (value subsets)
		 (declare (ignore value))
		 (incf sum (* (/ (length subsets) tags-lists-length)
			      (entropy target-attribute subsets))))
	     avp-sets)
     ;; Return the gain
     (- (entropy target-attribute tags-lists) sum)))

;;;-----------------------------------------------------------------------------
;;; HIGHEST-GAIN-ATTRIBUTE
;;;-----------------------------------------------------------------------------

;;; Returns as multiple values the highest the highest gain attribute
;;; and its gain.

(defun HIGHEST-GAIN-ATTRIBUTE (target-attribute attributes tags-lists)
  (let* ((attribute-gains
	  (sort (mapcar #'(lambda (attribute)
			    (list attribute
				  (gain target-attribute attribute tags-lists)))
			attributes)
		#'>
		:key #'second))
 	 (hga (find-if #'(lambda (e) (> e 0.0)) attribute-gains :key #'second)))
    (format t "~%Attribute-gains ~a" attribute-gains)
    (values  (or (avp-attribute hga) (first attributes))
	     (or (avp-value hga) 0.0))))

;;;-----------------------------------------------------------------------------
;;; HIGHEST-ENTROPY-ATTRIBUTE
;;;-----------------------------------------------------------------------------

;;; Return the attribute with the highest entropy.

(defun HIGHEST-ENTROPY-ATTRIBUTE (attributes tags-lists)
  (let ((entropy-list nil))
    (dolist (attr attributes)
      (multiple-value-bind (entropy avP-set)
	  (entropy attr tags-lists)
	(push (list attr entropy avP-set) entropy-list)))
    (setf entropy-list
      (sort entropy-list #'> :key #'second))
    ;;(format t "~%HGA list: ~a" entropy-list)
    (setf entropy-list
      (find-if #'(lambda (e) (> e 0.0)) entropy-list :key #'second))
    (values (first entropy-list)
	    (third entropy-list))))

;;;-----------------------------------------------------------------------------
;;; LOWEST-ENTROPY-ATTRIBUTE
;;;-----------------------------------------------------------------------------

;;; Return the attribute with the lowest entropy.

(defun LOWEST-ENTROPY-ATTRIBUTE (attributes tags-lists)
  (let ((entropy-list nil))
    (dolist (attr attributes)
      (multiple-value-bind (entropy avP-set)
	  (entropy attr tags-lists)
	(push (list attr entropy avP-set) entropy-list)))
    (setf entropy-list
      (sort entropy-list #'< :key #'second))
    ;;(format t "~%LGA list: ~a" entropy-list)
    (setf entropy-list
      (find-if #'(lambda (e) (> e 0.0)) entropy-list :key #'second))
    (values (first entropy-list)
	    (third entropy-list))))

;;;-----------------------------------------------------------------------------
;;; COMPUTE-ATTRIBUTE-VALUES
;;;-----------------------------------------------------------------------------

;;; This determines the unique set of values that a particular
;;; attribute <attribute> amongst the collection of attribute value
;;; pairs contained in tags-list.

(defun COMPUTE-ATTRIBUTE-VALUES (attribute tags-lists)
  (let ((attribute-values nil))
    (dolist (example tags-lists)
      (pushnew (find-attribute-value attribute example)
	       attribute-values :test #'equalp))
    attribute-values))

;;;-----------------------------------------------------------------------------
;;; PARTITION-DATA-BY-ATTRIBUTE
;;;-----------------------------------------------------------------------------

(defun PARTITION-DATA-BY-ATTRIBUTE (attribute examples)
  (let ((avp-sets (make-hash-table :test #'equalp)))
   (dolist (tags examples)
      (let ((value (or (second (find attribute tags :key #'first)) "NO-VALUE")))
	(setf (gethash value avp-sets)
	  (push tags (gethash value avp-sets)))))
   avp-sets))

;;;-----------------------------------------------------------------------------

(defun PRINT-DATA-PARTITIONS (avP-sets)
  (with-hash-table-iterator (next-entry avP-sets)
      (loop (multiple-value-bind (more value data-partition) (next-entry)
	      (unless more (return nil))
	      (format t "~%Tag: ~a ~a" value (length data-partition))))))

;;;*****************************************************************************
;;;
;;; PART II: The ID3 Algorithm
;;;
;;;*****************************************************************************

;;;*****************************************************************************
;;;
;;; ID3 (Examples, Target_Attribute, Attributes)
;;;    1.Create a root
;;;    2. If all examples are positive, return single root node, with label +
;;;    3. If all examples are negative, return single root node, with label -
;;;    4. If attribute is empty, return single root node, with label most
;;;       common value of Target_Attribute in Examples
;;;    5. Otherwise
;;;       a) Set A to attribute from Attributes that best classifies Examples
;;;       b) Set the decision attribute for root to A
;;;       c) For each value vi of A
;;;             1. Add a new tree branch below root, corresponding to test= vi
;;;             2. Let Examples[vi] be the subset of Examples with value vi for A
;;;                If Examples[vi] is empty
;;;                   Add a leaf node whose label is the most common value of
;;;                   Target_Attribute in Examples
;;;                Else below this branch add the subtree
;;;                   ID3 (Examples[vi] , Target_Attribute, Attributes - {A})
;;;    6. Return root
;;;
;;;
;;;*****************************************************************************


;;;-----------------------------------------------------------------------------
;;; ID3
;;;-----------------------------------------------------------------------------

(defun ID3 (target-attribute attributes examples
	    &optional
	    (root (make-dt-node "label")))
    (when (all-examples-positive-p examples)
      (set-node-value root "+")
      (set-node-label root "+")
      (return-from ID3 root))
    (when (all-examples-negative-p examples)
      (set-node-value root "-")
      (set-node-label root "-")
      (return-from ID3 root))
    (when (null attributes)
      (return-from ID3 root))
    ;; This is the core ID3 Algorithm
    (let* ((attribute (highest-gain-attribute target-attribute attributes examples))
	   (dt-branch nil)
	   (examples-vi nil))
      (set-node-value root attribute)
      (set-node-label root attribute)
      (dolist (vi (compute-attribute-values attribute examples))
	(setf dt-branch
	  (add-dt-branch root (make-dt-node vi (1+ (util::node-depth root)))))
	(setf examples-vi
	  (find-sets-with-attribute-value examples attribute vi))
	(cond ((null examples-vi)
	       (add-dt-leaf dt-branch (most-common-target-attribute-value examples)))
	      ((null attribute)
	       (error "Highest gain attribute returned no value!"))
	      (t
	       (setf attributes (remove attribute attributes))
	       (format t "~%Attribute: ~a = ~a, ~%Attributes ~a" attribute vi attributes)
	       (add-dt-branch dt-branch
			      (ID3 target-attribute
				   attributes
				   examples-vi
				   (make-dt-node "label"
						 (1+ (util::node-depth dt-branch))))))))
      root))

;;;-----------------------------------------------------------------------------

(defun ALL-EXAMPLES-POSITIVE-P (examples)
  (every #'(lambda (example)
	     (equal (second (first (last example))) 'Yes))
	 examples))

;;;-----------------------------------------------------------------------------

(defun ALL-EXAMPLES-NEGATIVE-P (examples)
   (every #'(lambda (example)
	     (equal (second (first (last example))) 'No))
	  examples))

;;;-----------------------------------------------------------------------------
;;; ALL-ATTRIBUTES
;;;-----------------------------------------------------------------------------

(defun ALL-DT-ATTRIBUTES (examples)
  (let ((example (cdr (reverse (first examples)))))
    (mapcar #'first example)))

;;;-----------------------------------------------------------------------------
;;; FIND-EXAMPLES-WITH-ATTRIBUTE-VALUE
;;;-----------------------------------------------------------------------------

(defun FIND-SETS-WITH-ATTRIBUTE-VALUE (examples attribute attribute-value)
  (let* ((examples-subset nil))
    (dolist (example examples)
      (when (equal (find-attribute-value attribute example)
		attribute-value)
	(push example examples-subset)))
    examples-subset))

;;;-----------------------------------------------------------------------------
;;; DT NODES
;;;-----------------------------------------------------------------------------

(defun MAKE-DT-NODE (label &optional (depth 0))
  (util::make-tree-node label :depth depth))

;;;-----------------------------------------------------------------------------

(defun SET-NODE-VALUE (node attribute)
  (setf (util::node-value node) attribute))

;;;-----------------------------------------------------------------------------

(defun SET-NODE-LABEL (node attribute)
  (setf (util::node-name node) attribute))

;;;-----------------------------------------------------------------------------

(defun ADD-DT-LEAF (node label)
  (util::add-child (util::make-tree-node label :depth (1+ (util::node-depth node)))
		   node))

;;;-----------------------------------------------------------------------------

(defun ADD-DT-BRANCH (node child)
  (util::add-child child node))

;;;-----------------------------------------------------------------------------
;;; EXAMPLES-WITH-VALUE-VI-FOR-A
;;;-----------------------------------------------------------------------------

(defun EXAMPLES-WITH-VALUE-VI-FOR-A (examples vi A)
  (declare (ignore examples vi A))
  nil)

;;;-----------------------------------------------------------------------------
;;; MOST-COMMON-VALUE-OF-TA
;;;-----------------------------------------------------------------------------

(defun MOST-COMMON-TARGET-ATTRIBUTE-VALUE (examples)
  (let* ((ta-counts nil)
	 (target-attribute nil)
	 (entry nil))
    (dolist (example examples)
      (setf target-attribute (second (first (last example))))
      (setf entry (find target-attribute ta-counts :key #'first :test #'eq))
      (cond (entry
	     (incf (second entry)))
	    (t
	     (push (list target-attribute 1) ta-counts))))
    (setf ta-counts (sort ta-counts #'>= :key #'second))
    (first (first ta-counts))))

;;;*****************************************************************************
;;; Part 3: Examples
;;;*****************************************************************************

;;;-----------------------------------------------------------------------------
;;; BOOLEAN-ENTROPIES
;;;-----------------------------------------------------------------------------

;;; This shows the range & distribution of entropy values in the boolean case

(defun BOOLEAN-ENTROPIES (sample-size)
  (format t "~%Boolean Entropy Ranges for sample size ~a:" sample-size)
  (dotimes (i sample-size)
    (let ((p i)
	  (n (- sample-size i)))
        (format t "~%Positive = ~a ~20tNegative = ~a, ~40tEntropy = ~a"
	      p
	      n
	      (boolean-entropy p n)))))


;;;-----------------------------------------------------------------------------
;;; THE PLAYTENNIS EXAMPLE FROM "Machine Learning"
;;;-----------------------------------------------------------------------------

;;; This is Table 3.2 (p. 59) of "Machine Learning" by Tom Mitchell. These are
;;; the training examples for the target concept Play-Tennis.

;;; Note: There are 36 (3x3x2x2) atrribute combinations. The target concept can be
;; aquired with these 14 examples.

(defparameter *PLAY-TENNIS-ATTRIBUTES* '(Outlook Temperature Humidity Wind))

(defparameter *PLAY-TENNIS-TARGET-ATTRIBUTE* 'Play-Tennis)

(defparameter *PLAY-TENNIS-TRAINING-EXAMPLES*
    '(((Outlook sunny)   (Temperature hot) (Humidity high)  (Wind weak)  (Play-Tennis No))
      ((Outlook sunny)   (Temperature hot) (Humidity high)  (Wind strong)(Play-Tennis No))
      ((Outlook overcast)(Temperature hot) (Humidity high)  (Wind weak)  (Play-Tennis Yes))
      ((Outlook rain)    (Temperature mild)(Humidity high)  (Wind weak)  (Play-Tennis Yes))
      ((Outlook rain)    (Temperature cool)(Humidity normal)(Wind weak)  (Play-Tennis Yes))
      ((Outlook rain)    (Temperature cool)(Humidity normal)(Wind strong)(Play-Tennis No))
      ((Outlook overcast)(Temperature cool)(Humidity normal)(Wind strong)(Play-Tennis Yes))
      ((Outlook sunny)   (Temperature mild)(Humidity high)  (Wind weak)  (Play-Tennis No))
      ((Outlook sunny)   (Temperature cool)(Humidity normal)(Wind weak)  (Play-Tennis Yes))
      ((Outlook rain)    (Temperature mild)(Humidity normal)(Wind weak)  (Play-Tennis Yes))
      ((Outlook sunny)   (Temperature mild)(Humidity normal)(Wind strong)(Play-Tennis Yes))
      ((Outlook overcast)(Temperature mild)(Humidity high)  (Wind strong)(Play-Tennis Yes))
      ((Outlook overcast)(Temperature hot) (Humidity normal)(Wind weak)  (Play-Tennis Yes))
      ((Outlook rain)    (Temperature mild)(Humidity high)  (Wind strong)(Play-Tennis No))))

(defparameter *AVP-LIST* (first *PLAY-TENNIS-TRAINING-EXAMPLES*))

;;;-----------------------------------------------------------------------------
;;; BOOLEAN-ENTROPY
;;;-----------------------------------------------------------------------------


(defmethod BOOLEAN-ENTROPY ((positive INTEGER)(negative INTEGER))
  (let* ((sample-size (+ positive negative))
	 (positive-proportion (/ positive sample-size))
	 (negative-proportion (/ negative sample-size)))
    (+ (- (* positive-proportion
	     (if (zerop positive-proportion) 0 (log positive-proportion 2))))
       (- (* negative-proportion
	     (if (zerop negative-proportion) 0 (log negative-proportion 2)))))))

;;;-----------------------------------------------------------------------------
;;; TEST-ID3-PLAY-TENNIS
;;;-----------------------------------------------------------------------------

(defun TEST-ID3-PLAY-TENNIS ()
  (let ((decision-tree (id3  *play-tennis-target-attribute*
			     *play-tennis-attributes*
			     *play-tennis-training-examples*)))
    (util::pretty-print-tree decision-tree)
    decision-tree))

;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
