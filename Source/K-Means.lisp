(in-package :ML)

;;;*******************************************************************
;;; The K-Means Algorithm
;;;*******************************************************************
;;
;;; Almost the simplest possible k-means implementation.
;;;
;;;*******************************************************************


;;;-------------------------------------------------------------------
;;; KMEANS (<data-array> :n-clusters :n-iterations :error-limit)
;;;-------------------------------------------------------------------
;;; 
;;; This is a simple implementation of basic kmeans.  It assumes that
;;; each row corresponds to one record of data. In other words,
;;; data-array has array-dimensions (n-columns n-rows).
;;;
;;;  Returns: 
;;;  (1) an array where each row is a cluster center.
;;;      this array has dimensions (n-columns n-clusters).
;;;  (2) a vector of cluster assignments, where the
;;;      ith assignment corresponds to the ith row
;;;      of the data-array. this has dimensions (n-rows).
;;;  (3) average error for the final clusters
;;;  (4) how many iterations were run.
;;;
;;; ALGORITHM
;;;
;;; 1. Assign a cluster-id to every row of data.  This is usually done
;;; with random assignments, since you usually don't know where the
;;; clusters are.  However, if you know something about the cluster
;;; centroids, you want to be able to initialize the centroids
;;; accordingly. We need to implement this feature.
;;;
;;; 2. Compute the cluster centroids based on the current cluster
;;; assignments (cluster-ids).
;;;
;;; 3. Re-assign every row of data to a new cluster-id based on which
;;; centroid is nearest to the row in Euclidean space.
;;;
;;; 4. Loop back to step 2 unless the total error of all the rows from
;;; their assigned centroids is below error-limit, or you've exceeded
;;; n-iterations.
;;;            
;;; ----------------------------------------------------------
;;; CONTINUE: This needs to be an object, so we can save its state and
;;; continue training if needed.
;;; ----------------------------------------------------------
;;; CONTINUE: want this to be a subfunction that is called with many
;;; random starting points, so we can save the best result (a la
;;; Matlab's kmeans).
;;; ----------------------------------------------------------
;;; CONTINUE: chop this up into reasonable sub-functions.
;;; ----------------------------------------------------------

;;;------------------------------------------------------------------------
;;; KMEANS
;;;------------------------------------------------------------------------

;;; Repetitively apply KMEANS-ESTIMATION and KMEANS-MAXIMIZATION to
;;; d-array, which is a column-major 2-D array of data, with the
;;; n parameters.  n-clusters defaults to 10, n-iterations to 100
;;; and error-limit to 0.001.  Returns several values: 
;;;
;;; 1. a column-major double array of cluster-centers whose dimensions
;;;    are (num-columns num-clusters)
;;; 2. a vector of cluster assignments for data-array.
;;; 3. The average error for the final iteration.
;;; 4. The final iteration number."
  
(defun KMEANS (data-array &key 
	       (n-clusters 10)
	       (n-iterations 100)
	       (error-limit 0.001))
  "Repetitively apply KMEANS-ESTIMATION and KMEANS-MAXIMIZATION to data-array."
  (let ((cluster-assignments (make-array (array-dimension data-array 1)
					 :element-type 'fixnum)))
    ;; first assign data to random clusters.  
    (dotimes (i (length cluster-assignments))
      (setf (aref cluster-assignments i) (random (floor n-clusters))))
    ;; check error-limit or iteration limit to see whether we're done
    (loop with average-error with cluster-centers
       with average-distance = most-positive-double-float
       for current-iteration from 0 below n-iterations
       while (> average-distance error-limit) do
	 ;; (format t "~A: ~A~%" current-iteration average-distance) (finish-output)
       ;; (E)stimation - learn the cluster centers given the cluster assignments.
	  (let ((new-cluster-centers
		 (kmeans-estimation data-array cluster-assignments n-clusters)))
	   (when cluster-centers
	     (setf average-distance 0)
	     (dotimes (i (array-total-size new-cluster-centers))
	       (incf average-distance (util::squared (- (row-major-aref cluster-centers i)
						  (row-major-aref new-cluster-centers i)))))
	     (setf average-distance (sqrt average-distance)))
	   (setf cluster-centers new-cluster-centers))
       ;; (M)aximization - re-assign the clusters according to the closest cluster centers.
	 (multiple-value-setq (cluster-assignments average-error)
	   (kmeans-maximization data-array cluster-centers))
       finally
	 (return (values cluster-centers cluster-assignments average-error current-iteration)))))



;;;------------------------------------------------------------------------
;;; KMEANS-ESTIMATION
;;;------------------------------------------------------------------------

(defun KMEANS-ESTIMATION (data-array cluster-assignments num-clusters)
  "Estimate the centers of num-clusters clusters, given the
   column-major double-valued data array and the given
   cluster-assignments (a vector of 0-based cluster indices).  Returns
   a column-major double array of cluster-centers whose dimensions
   are (num-columns num-clusters) whose values are the estimated
   centers."
  (loop with (columns rows) = (array-dimensions data-array)
     with cluster-centers-array = (make-array `(,columns ,num-clusters)
					      :element-type 'double-float :initial-element 0.0d0)
     for i below columns
     for data-vector     = (make-array rows :element-type (array-element-type data-array)
				       :displaced-to data-array
				       :displaced-index-offset (* i rows))
     for cluster-centers = (make-array num-clusters :element-type 'double-float
				       :displaced-to cluster-centers-array
				       :displaced-index-offset (* i num-clusters))
     do
       (kmeans-estimation-1 data-vector cluster-assignments num-clusters cluster-centers)
     finally (return cluster-centers-array)))

;;;------------------------------------------------------------------------

(defun KMEANS-ESTIMATION-1 (data-vector cluster-assignments num-clusters
			    &optional 
			    (cluster-centers
			     (make-array num-clusters :element-type 'double-float)))
  "Estimate the centers of n clusters, given the real-valued data
   vector and the given cluster-assignments (a vector of 0-based
   cluster indices).  cluster-centers (returned) is an array of
   doubles of length num-clusters, whose values get set to the
   estimated centers."
  (dotimes (i (length cluster-centers))
    (setf (aref cluster-centers i) 0.0d0))
  (let ((cluster-sizes (make-array (length cluster-centers)
				   :element-type 'fixnum :initial-element 0)))
    ;; compute centers based on cluster assignments
    (loop 
       for element across data-vector
       for current-cluster across cluster-assignments do
	 (incf (aref cluster-sizes current-cluster))
	 (incf (aref cluster-centers current-cluster)
	       element))
    ;; complete average of cluster centers by dividing by n per cluster
    (loop 
       for cluster-num below num-clusters
       for cluster-size across cluster-sizes
       unless (zerop cluster-size) do
	 (util::divf (aref cluster-centers cluster-num) cluster-size)))
  cluster-centers)

;;;------------------------------------------------------------------------
;;; KMEANS-MAXIMIZATION-1
;;;------------------------------------------------------------------------

(defun KMEANS-MAXIMIZATION-1 (row cluster-centers)
  "Computes a cluster assignment by minimum euclidean distance from
   the cluster-centers whose dimensions are (num-columns
   num-clusters).  Returns the assigned cluster, with its euclidian
   distance as a second value.  Used by KMEANS-MAXIMIZATION, can also
   be used to classify a row of data to a given cluster."
  (loop with best-cluster
     with best-distance = most-positive-long-float
     with (n-cols n-clusters) = (array-dimensions cluster-centers)
     for current-cluster below n-clusters
     for sum-of-diffs = 0.0d0 do
     ;; first compute the distance between the current-cluster and the
     ;; current-row
       (loop for col below n-cols
	  for element across row
	  do (incf sum-of-diffs
		   (util::squared (- (aref cluster-centers col current-cluster)
				     element))))
     ;; if this distance is better than previous best, update best
     ;; distance and cluster-assignment
       (when (< sum-of-diffs best-distance)
	 (setf best-distance sum-of-diffs
	       best-cluster current-cluster))
     ;; we return the square root of the best distance (because it's
     ;; euclidian distance).
     finally (return (values best-cluster (sqrt best-distance)))))

;;;------------------------------------------------------------------------
;;; KMEANS-MAXIMIZATION
;;;------------------------------------------------------------------------

(defun KMEANS-MAXIMIZATION (data-array cluster-centers-array)
  "Computes cluster assignment by minimum euclidean distance from the
   cluster-centers whose dimensions are (num-columns num-clusters).
   Returns the assigned clusters as a vector, with the average
   euclidian distance as a second value."
  (loop with average-error       = 0.0
     with    (n-columns n-rows)  = (array-dimensions data-array)
     with    vector-buffer       = (make-array n-columns :element-type (array-element-type data-array))
     with    cluster-assignments = (make-array n-rows :element-type 'fixnum)
     for row below n-rows
     for data-row = (array-row data-array row vector-buffer)
     do
       (multiple-value-bind (cluster error)
	   (kmeans-maximization-1 data-row cluster-centers-array)
	 (setf (aref cluster-assignments row) cluster)
	 (incf average-error error))
     ;; update average-error to really be the average error.
     finally (return (values cluster-assignments (/ average-error n-rows)))))


;;;------------------------------------------------------------------------
;;; KMEANS*
;;;------------------------------------------------------------------------

;; Old monolithic (and row-major) version.  Uses the transpose of
;; data-array above, and returns the transpose of cluster-centers.

(defun KMEANS* (data-array &key 
		(n-clusters 10)
		(n-iterations 100)
		(error-limit 0.001))
  (let* ((dims (array-dimensions data-array))
         (n-rows (first dims))
         (n-cols (or (second dims) 1)) ;; vectors have no second dimension
         (cluster-centers (make-array (list n-clusters n-cols) 
                                      :element-type 'double-float
                                      :initial-element 0.0D0))
         (cluster-sizes   (make-array n-clusters
                                      :element-type 'single-float
                                      :initial-element 0.0))
         (cluster-assignments (make-array n-rows
                                          :element-type 'fixnum 
                                          :initial-element -1))
         (current-error most-positive-long-float)
         (average-error  most-positive-long-float))
    (unless (and (numberp n-rows) (numberp n-cols)
		 (> n-rows 0)     (> n-cols 0)
		 (> n-clusters 1))
      (return-from kmeans*))
    ;; first assign data to random clusters.  
    ;; assignments range from 0 to n-clusters - 1.
    (loop for k below n-rows
       do (setf (aref cluster-assignments k) (random (floor n-clusters))))
    ;; algorithm core:
    ;;          check error-limit or iteration limit to see whether we're done
    (loop for current-iteration from 0 below n-iterations
       while (> average-error error-limit) do
       ;; initialize cluster-sizes and cluster-centers
	 (loop for cur-clust below n-clusters do
	    ;; reset cluster-size for this cluster
	      (setf (aref cluster-sizes cur-clust) 0.0)
	    ;; reset cluster-center for this cluster
	      (loop for each-col below n-cols
		 do (setf (aref cluster-centers cur-clust each-col) 0.0D0)))
       ;; compute centers based on cluster assignments
	 (loop for row below n-rows
	    for current-cluster = (aref cluster-assignments row) do
	      (incf (aref cluster-sizes current-cluster))
	      (loop for col below n-cols
		 do (incf (aref cluster-centers current-cluster col)
			  (aref data-array row col))))
       ;; complete average of cluster centers by dividing by n per cluster
	 (loop for current-cluster below n-clusters
	    do (loop for col below n-cols
		  unless (zerop (aref cluster-sizes current-cluster))
		  do (setf (aref cluster-centers current-cluster col)
			   (/ (aref cluster-centers current-cluster col)
			      (aref cluster-sizes current-cluster)))))
       ;; compute new cluster assignments by minimum euclidean distance
	 (setf average-error 0.0)
	 (loop for row below n-rows
	    for best-distance = most-positive-long-float do
	      (loop for current-cluster below n-clusters
		 for sum-of-diffs = 0.0D0 do
		 ;; first compute the distance between
		 ;; the current-cluster and the current-row
		   (loop for col below n-cols
		      do (incf sum-of-diffs
			       (util::squared (- (aref cluster-centers current-cluster col)
					   (aref data-array row col)))))
		 ;; if this distance is better than previous
		 ;; best, update best distance and 
		 ;; cluster-assignment
		   (when (< sum-of-diffs best-distance)
		     (setf best-distance sum-of-diffs)
		     (setf (aref cluster-assignments row) current-cluster)))
	    ;; update total error
	      (setf current-error (sqrt best-distance))
	      (incf average-error current-error))
       ;; update average-error to really be the average error.
	 (setf average-error (/ average-error (float n-rows)))
       finally
	 (return (values cluster-centers cluster-assignments average-error current-iteration)))))



;;;-------------------------------------------------------------------
;;; TRANSPOSE-ARRAY
;;;-------------------------------------------------------------------

(defun TRANSPOSE-ARRAY (2d-array)
  (loop with (dim1 dim2) = (array-dimensions 2d-array)
      with array = (make-array `(,dim2 ,dim1)
			       :element-type (array-element-type 2d-array))
     for i below dim1 do
       (dotimes (j dim2)
	 (setf (aref array j i) (aref 2d-array i j)))
     finally (return array)))

;;;-------------------------------------------------------------------
;;; ARRAY-ROW
;;;-------------------------------------------------------------------

(defun ARRAY-ROW (array row
		  &optional
		  (vector (make-array (array-dimension array 0)
				      :element-type (array-element-type array))))
  "Get a row from COLUMN-MAJOR array, using the (optionally supplied) vector."
  (dotimes (i (array-dimension array 0) vector)
    (setf (aref vector i) (aref array i row))))


;;;-------------------------------------------------------------------
;;; OLD CODE
;;;-------------------------------------------------------------------

;; old version.
#+IGNORE
(defun KMEANS (data-array &key 
	       (n-clusters 10)
	       (n-iterations 100)
	       (error-limit 0.001))
  (let* ((dims (array-dimensions data-array))
         (n-rows (first dims))
         (n-cols (or (second dims) 1)) ;; vectors have no second dimension
         (cluster-centers (make-array (list n-clusters n-cols) 
                                      :element-type 'double-float
                                      :initial-element 0.0D0))
         (cluster-sizes   (make-array n-clusters
                                      :element-type 'single-float
                                      :initial-element 0.0))
         (cluster-assignments (make-array n-rows
                                          :element-type 'fixnum 
                                          :initial-element -1))
         (current-error most-positive-long-float)
         (average-error   0.0)
         (current-iteration 0))
    (when (and (numberp n-rows)(numberp n-cols)
               (> n-rows 0)    (> n-cols 0)
               (> n-clusters 1))
      ;; first assign data to random clusters.  
      ;; assignments range from 0 to n-clusters - 1.
      (loop for k below n-rows
	 do (setf (aref cluster-assignments k) (random (floor n-clusters))))

      ;; algorithm core:
      ;;          check error-limit or iteration limit to see whether we're done
      (loop while (and (> current-error error-limit)
                       (< current-iteration n-iterations))
	 do  
	 (progn
	   (incf current-iteration)
	   ;; initialize cluster-sizes and cluster-centers
	   (loop for cur-clust below n-clusters
	      do (progn
		   ;; reset cluster-size for this cluster
		   (setf (aref cluster-sizes cur-clust) 0.0)
		   ;; reset cluster-center for this cluster
		   (loop for each-col below n-cols
		      do (setf (aref cluster-centers cur-clust each-col) 0.0D0))))
	   ;; compute centers based on cluster assignments
	   (loop for row below n-rows
	      do (let ((current-cluster (aref cluster-assignments row)))
		   (incf (aref cluster-sizes current-cluster))
		   (loop for col below n-cols
		      do (setf (aref cluster-centers current-cluster col)
			       (+ (aref cluster-centers current-cluster col)
				  (aref data-array row col))))))
	   ;; complete average of cluster centers by dividing by n per cluster
	   (loop for current-cluster below n-clusters
	      do (loop for col below n-cols
		    unless (zerop (aref cluster-sizes current-cluster))
		    do (setf (aref cluster-centers current-cluster col)
			     (/ (aref cluster-centers current-cluster col)
				(aref cluster-sizes current-cluster)))))
	   ;; compute new cluster assignments by minimum euclidean distance
	   (setf average-error 0.0)
	   (loop for row below n-rows
	      do (let ((best-distance most-positive-long-float))
		   (loop for current-cluster below n-clusters
		      do (let ((sum-of-diffs 0.0D0))
			   ;; first compute the distance between
			   ;; the current-cluster and the current-row
			   (loop for col below n-cols
			      do (setf sum-of-diffs
				       (+ sum-of-diffs
					  (util::squared (- (aref cluster-centers current-cluster col)
						      (aref data-array row col))))))
			   ;; if this distance is better than previous
			   ;; best, update best distance and 
			   ;; cluster-assignment
			   (when (< sum-of-diffs best-distance)
			     (setf best-distance sum-of-diffs)
			     (setf (aref cluster-assignments row) current-cluster))
			   ))
		   ;; update total error
		   (setf current-error (sqrt best-distance))
		   (setf average-error (+ average-error current-error))
		   ))
	   ;; update average-error to really be the average error.
	   (setf average-error (/ average-error (float n-rows)))
	   ))
      (values cluster-centers cluster-assignments current-error current-iteration))))

;;; ----------------------------------------------------------
;;; MAKE-KMEANS-TEST-DATA (n-rows n-cols)
;;; ----------------------------------------------------------

;;; Generate 2 clusters, one around (10, 10) and and another around
;;; (-10, -10).  Odd rows belong to one cluster and even rows belong
;;; to the other cluster.

(defvar *mean-1* 10.0)
(defvar *mean-2* -10.0)
(defvar *sd-1* 2.0)
(defvar *sd-2* 4.0)

;;;------------------------------------------------------------------------
;;; MAKE-KMEANS-TEST-DATA
;;;------------------------------------------------------------------------

(defun MAKE-KMEANS-TEST-DATA (&optional (n-rows 40) (n-cols 2))
  (let ((data-array (make-array (list n-rows n-cols) 
                                :element-type 'single-float 
                                :initial-element 0.0)))
    (loop for i from 0 below n-rows
          do
          (loop for j from 0 below n-cols
                do (cond ((oddp i)
                          (setf (aref data-array i j)
			    (util::gaussian-deviate *mean-1* *sd-1*)))
                         (t 
                          (setf (aref data-array i j)
			    (util::gaussian-deviate *mean-2* *sd-2*))))))
    data-array))

;;;------------------------------------------------------------------------
;;; End of File
;;;------------------------------------------------------------------------

