(in-package :ML)

(defvar *PTTE*  *PLAY-TENNIS-TRAINING-EXAMPLES*)
(defvar *attributes* (all-dt-attributes *ptte*))


;;;------------------------------------------------------------------------------------------
;;; SEQUENTIAL-COVERING
;;;------------------------------------------------------------------------------------------

;;; Repeated calls learn-one-rule until all examples are covered.

(defun SEQUENTIAL-COVERING (target-attribute attributes examples threshold)
  (let* ((rules nil)
         (new-rule (learn-one-rule target-attribute examples 3)))
    (loop
      (format t "~%New Rule: ~a" new-rule)
      (when (null examples)
        (return rules))
      (setf examples (filter-covered-examples new-rule examples))
      (setf new-rule (learn-one-rule target-attribute examples 3)))
    rules))

(defun all-constraints (examples target-attribute)
  (remove target-attribute
	  (remove-duplicates (apply #'append (copy-list examples))
			     :test #'equal)
	  :key #'first))

(defun findall (item sequence &key (test #'equal)(key #'identity))
  (let ((result nil))
    (mapc #'(lambda (x)
              (when (funcall test item (funcall key x))
                (push x result)))
          sequence)
    (nreverse result)))

(defun filter-covered-examples (rule examples)
  (remove-if #'(lambda (example)(rule-covers-example-p rule example)) examples))

;;;------------------------------------------------------------------------------------------
;;; FILTERING NEW HYPOTHESES
;;;------------------------------------------------------------------------------------------

(defun REMOVE-DUPLICATE-HYPOTHESES (hypotheses)
  (remove-duplicates (apply #'append (copy-list hypotheses)) :test #'equal))

(defun REMOVE-INCONSISTENT-HYPOTHESES (hypotheses)
  (let ((consistent-hypotheses nil))
    (dolist (hypothesis hypotheses)
      (let ((predicates-seen (make-hash-table :test #'eq :size 10))
            (consistent-p t))
        (dolist (constraint hypothesis)
          (if (gethash (avp-attribute constraint) predicates-seen)
              (setf consistent-p nil)
            (setf (gethash (avp-attribute constraint) predicates-seen) (avp-value constraint))))
        (when consistent-p
          (push hypothesis consistent-hypotheses))))
    consistent-hypotheses))

(defun FIND-SIMILAR-HYPOTHESES (h hs)
  (let ((attrs (mapcar #'avp-attribute h)))
    (remove-if-not #'(lambda (h1)
                       (every #'(lambda (a)(find a h1 :key #'avp-attribute :test #'eq))
                              attrs))
                   hs)))

;;;------------------------------------------------------------------------------------------
;;; MAXIMALLY SPECIFIC HYPOYTHESES
;;;------------------------------------------------------------------------------------------

(defun MAXIMALLY-SPECIFIC-HYPOTHESIS-P (new-hypothesis old-hypotheses)
  (let ((similar-hypotheses (find-similar-hypotheses new-hypothesis old-hypotheses)))
    ;;(print similar-hypotheses)
    (every #'(lambda (h)(< (length h)(length new-hypothesis)))
           similar-hypotheses)))

;;;------------------------------------------------------------------------------------------

;;; Removes from new-hypotheses those hypothese that are maxinally specifoc wrt old-hypotheses,

(defun REMOVE-NON-MAXIMALLY-SPECIFIC-HYPOTHESES (old-hypotheses new-hypotheses)
  (remove-if-not #'(lambda (h)
                     (maximally-specific-hypothesis-p h old-hypotheses))
                 new-hypotheses))

;;;------------------------------------------------------------------------------------------
;;; SPECIALIZE-HYPOTHESES
;;;------------------------------------------------------------------------------------------

;;; This first augments every h(i) with c(j) and then filters out new hypotheses that:

;;; 1. Are duplicates wrt <hypotheses>
;;; 2. Contain inconsitencies
;;; 3. Are not maximally specific wrt <hypotheses>

(defun SPECIALIZE-HYPOTHESES (hypotheses constraints)
  (let ((specialized-hypotheses
	 (mapcar #'(lambda (hypothesis)
		     (mapcar #'(lambda (constraint)
				 (remove-duplicates (cons constraint hypothesis) :test #'equal))
			     constraints))
		 hypotheses)))
    (remove-non-maximally-specific-hypotheses
     hypotheses
     (remove-inconsistent-hypotheses
      (remove-duplicate-hypotheses specialized-hypotheses)))))

(defun count-value-occurences (attribute examples)
  (let ((counts (make-hash-table :test #'eq :size 10)))
    (dolist (example examples)
      (let* ((value  (avp-value (find attribute example :key #'first)))
             (entry (gethash value counts)))
        (if entry
            (incf  (gethash value counts))
          (setf  (gethash value counts) 1))))
    (util::hatal counts)))

(defun most-frequently-occuring-value (attribute examples)
  (first (first (sort  (count-value-occurences attribute examples) #'> :key #'cdr))))

;;;------------------------------------------------------------------------------------------
;;; COMPUTE-HYPOTHESES-PERFORMANCE
;;;------------------------------------------------------------------------------------------

;;; Returns a sorted list of pairs consiting of hypothesis and performance.

(defun COMPUTE-HYPOTHESES-PERFORMANCE (hypotheses examples target-attribute)
  (sort #'> (mapcar #'(lambda (hypothesis)
			(append hypothesis
				(list (performance hypothesis examples target-attribute))))
		    hypotheses)
	:key #'third))

;;;------------------------------------------------------------------------------------------
;;; LEARN-ONE-RULE
;;;------------------------------------------------------------------------------------------

(defun LEARN-ONE-RULE (target-attribute examples k)
  (let* ((attributes (attributes-in-examples examples))
         (best-hypothesis '())
         (candidate-hypotheses `(,best-hypothesis))
         (constraints nil)
         (new-candidates nil))

    (loop
      (when (null candidate-hypotheses)
	(return t))

       (format t "~%Number of candidate hypothes: ~a"  (length candidate-hypotheses))
       ;; 1. Generate the next most specific candidate hypotheses
       (setf constraints (all-constraints examples target-attribute))
       (setf new-candidates
	 (specialize-hypotheses candidate-hypotheses constraints))

       ;; 2. Update Best hypothesis
       (dolist (hypothesis  new-candidates)
	 (cond ((null best-hypothesis)
		(setf best-hypothesis hypothesis))
	       (t
		(let ((p1 (performance hypothesis examples target-attribute))
		      (p2 (performance best-hypothesis examples target-attribute)))
		  (when (> p1 p2)
		    (setf best-hypothesis hypothesis))))))

       ;; 3. Update Candidate hypotheses by taking thhe k best members
       (setf candidate-hypotheses
         (util::nthcar k (mapcar #'first
                                 (performances new-candidates examples target-attribute)))))


     ;; 4. Returnn a rule of the form if <best-hypothesis> then <predication>
     ;;    where <predication> is the most frequent value of target-attribute among examples.
     (cons (list target-attribute (most-frequently-occuring-value target-attribute examples))
           best-hypothesis)))

(defun performances (hypotheses examples target-attribute)
  (sort (mapcar #'(lambda (hypothesis)
                    (list hypothesis (performance hypothesis examples target-attribute)))
                hypotheses)
        #'(lambda (x y)(< (second x)(second y)))))

(defun attribute-occurences (attribute examples)
  (let ((occurences (make-hash-table :test #'eq))
        (result nil))
    (mapc #'(lambda (example)
             (let* ((value (find-attribute-value attribute example))
                    (entry (gethash value occurences)))
               (if entry (incf (gethash value occurences))
                 (setf (gethash  value occurences) 1))))
      examples)
    (maphash #'(lambda (k v)(push (list k v) result)) occurences)
    result))

(defun most-frequent-value (attribute examples)
  (first (first (sort (attribute-occurences attribute examples)
                      #'(lambda (x y)(> (second x)(second y)))))))

(defun ATTRIBUTES-IN-EXAMPLES (examples)
  (remove-duplicates
   (apply #'append
          (mapcar #'(lambda (example)(attributes-in-constraints example))
                  examples))))

(defun ATTRIBUTES-IN-CONSTRAINTS (constraints)
  (mapcar #'avp-attribute constraints))

(defun attribute-value-in-constraints (attribute constraints)
  (avp-value (find attribute constraints :key #'first)))

(defun RULE-COVERS-EXAMPLE-P (rule example)
  (let ((attributes (attributes-in-constraints rule)))
    (every #'(lambda (attribute)
               (equal (attribute-value-in-constraints attribute rule)
                      (attribute-value-in-constraints attribute example)))
           attributes)))

;;; ------------------------------------------------------------------------
;;; Performance
;;; ------------------------------------------------------------------------

(defun HYPOTHESIS-MATCHES-EXAMPLE-P (h example)
  (every #'(lambda (conjunct)
             (let ((attribute (avp-attribute conjunct)))
               (equal (avp-value conjunct)
                      (attribute-value-in-constraints attribute example))))
         h))

(defun POSITIVE-EXAMPLE-P (target-attribute example &optional (positive-value 'yes))
  (equal (attribute-value-in-constraints target-attribute example) positive-value))

;;; ------------------------------------------------------------------------

(defun PERFORMANCE (h examples target-attribute)
  (let ((h-examples (remove-if-not #'(lambda (example)
				       (hypothesis-matches-example-p h example))
				   examples)))
    (- (entropy target-attribute h-examples))))

;;; ------------------------------------------------------------------------
;;; End of File
;;; ------------------------------------------------------------------------
