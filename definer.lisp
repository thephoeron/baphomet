;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :baphomet)

#-lispworks
(defparameter *user-package* :cl-user)

(defparameter *language* :common-lisp)

;;; ABSTRACT DEFINER DECLARATIONS

(defclass definer-class (funcallable-standard-class)
  ((name :initarg :definer-name :accessor definer-name-of)
   (options :initarg :definer-options :accessor definer-options-of)
   (signature :initarg :definer-signature :accessor definer-signature-of)
   (expansion-env :initarg :definer-expansion-env :accessor definer-expansion-env-of)
   (call-form :initarg :definer-call-form :accessor definer-call-form-of)
   (forms :initarg :definer-forms :accessor definer-forms-of))
  (:documentation "Generic class holding definer meta information."))

(defmethod validate-superclass ((class definer-class) (superclass funcallable-standard-class))
  t)

(defclass definer (funcallable-standard-object)
  ((name :initarg :name :accessor name-of)
   (options :initarg :options :accessor options-of)
   (signature :initarg :signature :accessor signature-of)
   (expansion-env :initarg :expansion-env :accessor expansion-env-of)
   (call-form :initarg :call-form :accessor call-form-of)
   (forms :initarg :forms :accessor forms-of))
  (:metaclass definer-class))

(defmethod validate-superclass ((class definer) (superclass funcallable-standard-object))
  t)

(def:print-object-method (definer :identity nil) (self stream)
  (format stream "definer ~s" (name-of self)))

(defun get-definer (name)
  "Returns definer class for the supplied definer of NAME."
  (or (find-class (intern (format nil "~:@(~a-definer~)" name)) :errorp nil)
      (error "Unknown definer: `~a'." name)))

(defgeneric available-definer-options (definer)
  (:documentation "List of available options for related definer."))

(defgeneric restricted-definer-options (definer)
  (:documentation
   "List of restricted option combinations for related definer."))

(defgeneric initialize-definer (definer)
  (:documentation "Initializes related definer class slots."))

(defmethod initialize-definer ((self definer)))

(defgeneric expand-definer (definer)
  (:documentation "Expands related definer class into its compilation form."))

(defmethod expand-definer ((self definer)))

#+nil
(defmacro definer (name-and-options &rest rest)
  (destructuring-bind (name &rest definer-options) (ensure-list name-and-options)
    (expand-definer
     (initialize-definer (make-instance (get-definer name)
                                        :definer-options definer-options
                                        :forms rest)))))

;; I could increase the generality of definer forms if I delay deconstruction of
;; signatures until the definer being defined gets it
(defmacro def:definer (&whole whole &environment environment
                       type-name definer-options
                       (&optional eval-type-op instance-name typed-lambda-list return-type)
                       &body body
                       &aux (signature (signature eval-type-op instance-name typed-lambda-list return-type))
                            (language (ensure-symbol (package-name (symbol-package type-name)) :keyword))
                            (definer-name (format nil "~A-DEFINER" (symbol-name type-name))))
  (declare (symbol type-name eval-type-op instance-name return-type)
           (vector definer-options)
           (list typed-lambda-list))
  `(progn
     (defparameter *current-package-name* (package-name *package*))
     (defparameter *current-language-name* (package-name *language*))
     (in-package *user-package*)
     (defpackage ,(symbol-name type-name)
       (:nicknames ,(format nil "~A.TYPES.~A" (package-name language) (symbol-name type-name))
                   ,(format nil "CL.TYPE.~A" (symbol-name type-name)))
       (:use cl baphomet))
     (in-package ,(symbol-name type-name))
     (setf *language* language)
     (defparameter *defining-form*
       ',whole)
     (defparameter *defining-environment*
       ',environment)
     (defclass ,(ensure-symbol definer-name) (baphomet:definer)
       ()
       (:metaclass baphomet:definer-class)
       (:definer-name 'def:definer)
       (:definer-options ,definer-options)
       (:definer-signature ,signature)
       (:definer-expansion-env ,environment)
       (:definer-call-form ,whole)
       (:definer-forms ,body))
     (defmethod new (&rest initargs)
       (apply #'make-instance (cons ',(get-definer type-name) initargs)))
     (defmethod available-definer-options ((definer ,(ensure-symbol definer-name)))
       ,(coerce definer-options 'list))
     (defmethod expand-definer ((definer ,(ensure-symbol definer-name)))
       ,(assoc-val :expander body))
     (defmethod initialize-definer ((definer ,(ensure-symbol definer-name)))
       ,(assoc-val :initializer body))
     ,@(when (member #\e definer-options :test #'char-equal)
         `(export (make-symbol ,(symbol-name type-name)) :def))
     (defmacro ,(ensure-symbol type-name :def) (&whole whole &environment environment (&rest options) signature &body body)
       (expand-definer
         (initialize-definer (new :definer-options options :signature signature
                                  :expansion-env environment :call-form whole
                                  :forms body))))
     (in-package *current-package-name*)
     (setf *language* (ensure-symbol *current-language-name* :keyword))))

;; (defmacro def:definition (signature (&key options) &body body)
;;   `(progn
;;      (defun ,(ensure-symbol (name-of signature :def)) ,(lambda-list-of signature)
;;        ))
;;      ,(expand-definer
;;         (initialize-definer (make-instance (get-definer (name-of signature))
;;                                                         :definer-options options
;;                                                         :forms body))))

;;; COMMON ROUTINES

(defun definer-type (definer)
  (class-name (class-of definer)))

(defclass keyword-definer (definer) ()
  (:metaclass definer-class))
