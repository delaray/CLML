(in-package :PQ)

;;;*********************************************************************************
;;; The A* Algorithm
;;;*********************************************************************************

;;; The code in this file implements the Standard A* as described in
;;; Chapter 3 of "Artificial Intelligence", first edition, by Elaine
;;; Rich.

;;; A description of the algorithm as well pseudo-code implementation
;;; Rich's description can be found
;;; ~/Meperia/Documents/Parser/Heuristic-Parsing.docx, this will most
;;; likely be moved to another document eventually.

;;;*********************************************************************************
;;; 
;;; Classes
;;; -------
;;;
;;; A*-Node
;;;
;;;
;;; Functions
;;; ---------
;;;
;;; MAKE-A*-NODE
;;; A*
;;; MIN-COST-NODE
;;; GOAL-NODE-P
;;; SUCCESSORS
;;; ADD-SUCCESSOR
;;;
;;; Test Code
;;; ---------
;;;
;;; GFN
;;; HFN
;;; INIT-TREE1
;;; INIT-TREE2
;;;
;;;*********************************************************************************


;;;----------------------------------------------------------------------------
;;; A*-NODE
;;;----------------------------------------------------------------------------

;;; This is used a wrapper node during the process of the A*
;;; algorithm. Its main purposeis toprovide a convenient place to
;;; cache the g & h' values of each node.

;;; The value slot simply holds the original node.

(defclass A*-NODE (VALUE)
  ((value :initarg :node
	  :initform nil
	  :accessor object-node)
   (g-value :initarg :g-value
	    :initform 0
	    :accessor object-g-value)
   (f-prime-value :initform 0
		  :initarg :f-prime-value
		  :accessor object-f-prime-value)
   (parent :initarg :parent
	   :initform nil
	   :accessor object-parent)
   (children :initarg :children
	   :initform nil
	   :accessor object-children)))

;;;----------------------------------------------------------------------------

;;; This allows A* nodes to print out like regular nodes.

(defmethod PRINT-OBJECT ((obj A*-NODE) stream)
  (cond (*print-readably*
	 (call-next-method))
	(t
	 (let ((node (object-node obj)))
	   (format stream "#<A* ~a: Value= ~a, Depth= ~a>"
		   (object-name obj)
		   (node-value node)
		   (node-depth node))))))

;;;----------------------------------------------------------------------------

;;; This creates an A* node.

(defun MAKE-A*-NODE (node g-value &optional (f-prime-value 0))
  (make-instance 'A*-NODE
    :node node
    :g-value g-value
    :f-prime-value f-prime-value))
		 
;;;----------------------------------------------------------------------------
;;; A*
;;;----------------------------------------------------------------------------

;;; This is the most general version of the algorithm in that it is
;;; not limited to trees, but actually handles general graphs, in
;;; particular cycles are handled.

;;; As with all Best-First search algorithms, only a near-optimal
;;; solution is guaranteed.

(defmethod A* ((root-node NODE) g-fn h-prime-fn)
  (let ((open-nodes
	 (list (make-a*-node root-node 0 (+ (funcall g-fn root-node nil)
					    (funcall h-prime-fn root-node)))))
	(close-nodes nil)
	(bestnode nil)
	(old nil)
	(goal-node nil))
    (loop 
      ;; If werun out of nodes and have not found a solution, then FAIL.
      (cond ((null open-nodes)
	     (return 'FAILURE))
	    (t
	     ;; Otherwise it's business as usual. Proceed with best-first search.
	     (setf bestnode (min-cost-node open-nodes nil g-fn h-prime-fn))
	     (format t "~%Open Nodes:~%")
	     (mapc #'print open-nodes)
	     (format t "~%BestNode: ~a" bestnode)
	     (setf open-nodes (remove (object-node bestnode) open-nodes
				      :test #'eq
				      :key #'object-node))
	     (setf close-nodes (append close-nodes (list bestnode)))
	     #+IGNORE
	     (format t "~%Bestnode: ~a~%Open-Nodes:~a~%Close-Nodes:~a"
		     bestnode open-nodes close-nodes)
	     (cond ((goal-node-p bestnode)
		    (return 'SUCCESS))
		   (t 
		    (setf xx bestnode)
		    (dolist (successor (successors bestnode))
		      (setf successor 
		       (make-instance 'A*-NODE
			 :name (slot-value successor 'name)
			 :node successor
			 :g-value (+ (object-g-value bestnode)
				     (node-cost successor bestnode))
			 :parent bestnode))
		      (pushnew successor (object-children bestnode))
		      #+IGNORE
		      (format t "~%Successor: ~a" successor)
		      (cond ((member (object-node successor) open-nodes :key #'object-node)
			    (setf old 
			      (find (object-node successor) open-nodes :key #'object-node))
			    (when (< (object-g-value successor)(object-g-value old))
			      (setf (object-parent old)(object-parent successor))))
			   ((member (object-node successor) close-nodes :key #'object-node)
			    (setf old 
			      (find (object-node successor) close-nodes :key #'object-node))
			    (when (< (object-g-value successor)(object-g-value old))
			      (setf (object-parent old)(object-parent successor))
			      (setf (object-g-value old)
				(object-g-value successor))
			      (setf (object-f-prime-value old)
				(object-f-prime-value successor))
			      ;; This is where dfs propagation needs to happen.
			      (dfs-propagate old h-prime-fn nil)))
			   (t
			    (setf open-nodes (append open-nodes (list successor)))
			    (add-successor successor bestnode)
			    (setf (object-f-prime-value successor)
			      (+ (object-g-value successor)
				 (funcall h-prime-fn successor)))))))))))
    (values bestnode close-nodes)))

;;;----------------------------------------------------------------------------
;;; DFS-PROPAGATE
;;;----------------------------------------------------------------------------

(defmethod DFS-PROPAGATE ((node A*-NODE) h-prime-fn visited)
  (dolist (successor (successors node))
    (unless (member successor visited :test #'eq)
      (push successor visited)
      (setf (object-g-value successor)
	(+ (object-g-value node)(node-cost successor node)))
      (setf (object-f-prime-value successor)
	(+ (object-g-value successor)
	   (funcall h-prime-fn successor)))
      (setf visited
	(dfs-propagate successor h-prime-fn visited))))
  visited)

;;;----------------------------------------------------------------------------
;;; MIN-COST-NODE
;;;----------------------------------------------------------------------------

(defmethod MIN-COST-NODE ((node TREE-NODE)(parent NULL) g-fn h-prime-fn)
  (min-cost-node (node-children node) node g-fn h-prime-fn))

;;;----------------------------------------------------------------------------

(defmethod MIN-COST-NODE ((nodes LIST)(parent t) g-fn h-prime-fn)
  (let ((min-cost-node (first nodes)))
    (dolist (node (rest nodes))
      (unless parent
	(setf parent (object-parent node)))
      (when (<  (+ (funcall g-fn (object-node node) parent)
		   (funcall h-prime-fn (object-node node)))
		(+ (funcall g-fn (object-node min-cost-node) parent)
		   (funcall h-prime-fn (object-node min-cost-node))))
	(setf min-cost-node node)))
    min-cost-node))

;;;----------------------------------------------------------------------------
;;; GOAL-NODE-P
;;;----------------------------------------------------------------------------

(defmethod GOAL-NODE-P ((node A*-NODE))
  (null (successors node)))

;;;----------------------------------------------------------------------------
;;; SUCCESSORS & PREDECESSORS (A*-NODE)
;;;----------------------------------------------------------------------------

(defmethod SUCCESSORS ((node A*-NODE))
  (successors (object-node node)))

;;;----------------------------------------------------------------------------

(defmethod PREDECESSORS ((node A*-NODE))
  (predecessors (object-node node)))

;;;----------------------------------------------------------------------------
;;; SUCCESSORS & PREDECESSORS (TREE-NODE)
;;;----------------------------------------------------------------------------

(defmethod SUCCESSORS ((node TREE-NODE))
  (node-children node))

;;;----------------------------------------------------------------------------

(defmethod ADD-SUCCESSOR ((successor A*-NODE) (node A*-NODE))
  (pushnew successor (object-children node))
  (setf  (object-parent successor) node))

;;;----------------------------------------------------------------------------

(defmethod ADD-SUCCESSOR ((node TREE-NODE) (successor TREE-NODE))
  (add-child successor node))

;;;----------------------------------------------------------------------------

(defmethod PREDECESSORS ((node TREE-NODE))
  (list (node-parent node)))

;;;----------------------------------------------------------------------------

(defmethod NODE-COST ((node TREE-NODE)(parent TREE-NODE))
  (object-value node))

;;;----------------------------------------------------------------------------

(defmethod NODE-COST ((node NODE)(parent NULL))
  0)

;;;----------------------------------------------------------------------------
;;; SUCCESSORS & PREDECESSORS (DAG-NODE)
;;;----------------------------------------------------------------------------

(defmethod SUCCESSORS ((node DAG-NODE))
  (node-children node))

;;;----------------------------------------------------------------------------

(defmethod ADD-SUCCESSOR ((node DAG-NODE) (successor DAG-NODE))
  (add-child successor node))

;;;----------------------------------------------------------------------------

(defmethod PREDECESSORS ((node DAG-NODE))
  (node-parents node))

;;;----------------------------------------------------------------------------

(defmethod NODE-COST ((node DAG-NODE)(parent DAG-NODE))
  (let ((n (or (position parent (node-parents node)) 0)))
    (nth n (object-value node))))

;;;----------------------------------------------------------------------------

(defmethod NODE-COST ((node A*-NODE)(parent A*-NODE))
  (node-cost (object-node node)(object-node parent)))

;;;----------------------------------------------------------------------------

(defmethod NODE-COST ((node NODE)(parent A*-NODE))
  (node-cost node(object-node parent)))

;;;----------------------------------------------------------------------------
;;; SIMPLE TEST CASES
;;;----------------------------------------------------------------------------

;;; Use the node value as the known cost.

(defparameter GFN #'(lambda (node parent)(node-cost node parent)))

;;;----------------------------------------------------------------------------

;;; Use 1 as the estimated cost forward.

(defparameter HFN #'(lambda  (node)(declare (ignore node)) 1))

;;;----------------------------------------------------------------------------

;;; This builds a complete binary of depth 3 in which the leftmost
;;; branch is the lowest cost path.

(defun INIT-TREE1 ()
  (let* ((tree1 (make-instance 'TREE-NODE :name 'ROOT :value 1 :depth 0))
	 (child1 
	  (add-child (make-instance 'TREE-NODE :name 'NODE-1 :value 2 :depth 1) tree1))
	 (child2
	  (add-child (make-instance 'TREE-NODE :name 'NODE-2 :value 3 :depth 1) tree1)))
    (add-child (make-instance 'TREE-NODE :name 'LEAF-1 :value 4 :depth 2) child1)
    (add-child (make-instance 'TREE-NODE :name 'LEAF-2 :value 5 :depth 2) child1)
    (add-child (make-instance 'TREE-NODE :name 'LEAF-3 :value 6 :depth 2) child2)
    (add-child (make-instance 'TREE-NODE :name 'LEAF-4 :value 7 :depth 2) child2)
    tree1))

;;;----------------------------------------------------------------------------

;;; This builds a complete binary of depth 3 in which the rightmost branch is the
;;; lowest cost path.

(defun INIT-TREE2 ()
  (let* ((tree2 (make-instance 'TREE-NODE :name 'ROOT :value 1 :depth 0))
	 (child1 
	  (add-child (make-instance 'TREE-NODE :name 'NODE-1 :value 7 :depth 1) tree2))
	 (child2
	  (add-child (make-instance 'TREE-NODE :name 'NODE-2 :value 6 :depth 1) tree2)))
    (add-child (make-instance 'TREE-NODE :name 'LEAF-1 :value 5 :depth 2) child1)
    (add-child (make-instance 'TREE-NODE :name 'LEAF-2 :value 4 :depth 2) child1)
    (add-child (make-instance 'TREE-NODE :name 'LEAF-3 :value 3 :depth 2) child2)
    (add-child (make-instance 'TREE-NODE :name 'LEAF-4 :value 2 :depth 2) child2)
    tree2))

;;;----------------------------------------------------------------------------

(defun INIT-DAG ()
  (let* ((root
	  (make-instance 'DAG-NODE :name 'ROOT :value (list 1) :depth 0))
	 (node1
	  (add-child (make-instance 'DAG-NODE :name 'NODE-1 :value (list 1) :depth 1)
		     root))
	 (node2
	  (add-child (make-instance 'DAG-NODE :name 'NODE-2 :value (list 2) :depth 1)
		     root))
	 (node3
	  (add-child (make-instance 'DAG-NODE :name 'NODE-3 :value  (list 3) :depth 2)
		     node1))
	 (node4
	  (add-child (make-instance 'DAG-NODE :name 'NODE-4 :value  (list 4) :depth 2)
		     node1))
	 (node5
	  (add-child (make-instance 'DAG-NODE :name 'NODE-5 :value  (list 5) :depth 2)
		     node2))
	 (node6
	  (add-child (make-instance 'DAG-NODE :name 'NODE-6 :value  (list 6) :depth 2)
		     node2))
	 (node7
	  (add-child (make-instance 'DAG-NODE :name 'NODE-7 :value  (list 7) :depth 3)
		     node3))
	 (node8
	  (add-child (make-instance 'DAG-NODE :name 'NODE-8 :value  (list 4) :depth 3)
		     node5))
	 (goal 
	  (add-child (make-instance 'DAG-NODE :name 'GOAL :value  (list 7) :depth 4)
		     node7)))
    ;; Add the additional parents.
    (add-child node7 node4)
    (setf (object-value node7)(append (object-value node7) '(5)))
    (add-child node8 node6)
    (setf (object-value node8)(append (object-value node8) '(6)))
    (add-child goal node8)
    (setf (object-value goal)(append (object-value goal) '(2)))
    ;; Return the Root node
    root))
  
;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------