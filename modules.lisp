(in-package :module-manager)

;;; ===========================================================================
;;;
;;; Author: Raymond de Lacaze
;;;
;;; This file defines the GRAPH module
;;;
;;; ===========================================================================

;;;----------------------------------------------------------------------------
;;; UTILITIES Package
;;;----------------------------------------------------------------------------

(eval-when (eval compile load)
  (unless (find-package :machine-learning)
    (make-package "MACHINE-LEARNING"
		  :use '(#-SBCL :user #+SBCL :cl-user :common-lisp #-SBCL
			 :clos #-SBCL :excl)
		  :nicknames '(:ml))))


(define-root-directory '(:machine-learning) *load-truename*)

;;;----------------------------------------------------------------------------
;;; Global ML Pathname Related Variables
;;;----------------------------------------------------------------------------

(defparameter *ML-DEVICE*
    (pathname-device *load-truename*))

;;;----------------------------------------------------------------------------

(defparameter *ML-DIRECTORY*
   (pathname-directory *load-truename*))

;;;----------------------------------------------------------------------------

(defparameter *ML-ROOT*
    (make-pathname :name nil :type nil
                   :directory  (pathname-directory *load-truename*)
                   :defaults  *load-truename*))


;;;----------------------------------------------------------------------------
;;; ML
;;;----------------------------------------------------------------------------

(define-module :ml
  (:directory :machine-learning)
  (:requires :utilities)
  (:files "Attribute-Value"
	  "Decision-Trees"
	  "Sequential-Covering"
	  "K-Means"
	  "Backprop"
	  "Linear-Regression"
          #+IGNORE
	  "ML-Class"
	  #+IGNORE
	  "A-Star"))


;;;-------------------------------------------------------------------------
;;; End of File
;;;-------------------------------------------------------------------------
