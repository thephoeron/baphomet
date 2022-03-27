;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :baphomet)
;;; COMMON VARIABLES

(defclass variable-definer (definer)
  ((value :accessor value-of)
   (documentation :accessor documentation-of :initform nil))
  (:metaclass definer-class))

(defmethod available-definer-options ((definer variable-definer))
  (list #\e))

(defmethod restricted-definer-options ((definer variable-definer))
  nil)


;;; COMMON ROUTINES

(defun initialize-variable-like-definer (definer &optional extra-options-writer)
  (destructuring-bind (name &optional (value nil value-p))
      (forms-of definer)
    (setf (name-of definer) name)
    (when value-p (setf (value-of definer) value)))
  (validate-definer-options definer extra-options-writer)
  definer)

(defun validate-constant-initial-value (definer)
  (unless (slot-boundp definer 'value)
    (error "Try to define ~s of type ~s without an initial value."
           (name-of definer) (definer-type definer))))

(defun initialize-and-validate-variable-like-definer
    (definer &optional extra-option-writer-keywords)
  (prog1 (initialize-variable-like-definer
          definer (if extra-option-writer-keywords
                      (combine-option-writers extra-option-writer-keywords)))
    (validate-constant-initial-value definer)))


;;; CONSTANT DEFINER ROUTINES

(defclass constant-definer (variable-definer)
  ((test-function :accessor test-function-of :initform 'eql))
  (:metaclass definer-class))

(defmethod initialize-definer ((definer constant-definer))
  (initialize-and-validate-variable-like-definer
   definer
   (list
    :documentation (make-option-writer documentation-of ensure-string-option)
    :test (make-option-writer test-function-of ensure-function-option))))

(defun reevaluate-constant (definer)
  (let ((name (name-of definer))
        (new-value (value-of definer)))
    `(if (not (boundp ',name))
         ,new-value
         ,(let ((old-value name)
                (test-function (ensure-function (test-function-of definer))))
            `(cond ((not (constantp ',name))
                    (cerror "Try to redefine the variable as a constant."
                            "~@<~s is an already bound non-constant variable ~
                             whose value is ~s.~:@>"
                            ',name ,old-value))
                   ((not (,test-function ,old-value ,new-value))
                    (cerror "Try to redefine the constant."
                            "~@<~s is an already defined constant whose value ~
                             ~s is not equal to the provided initial value ~s ~
                             under ~s.~:@>"
                            ',name ,old-value ',test-function)
                    ,new-value)
                   (t ,old-value))))))

(defmethod expand-definer ((definer constant-definer))
  (let ((name (name-of definer)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defconstant ,name ,(reevaluate-constant definer)
           ,@(when-let (documentation (documentation-of definer))
               `(,documentation))))
       ,@(when (has-option-p definer #\e) `((export ',name))))))


;;; LOAD-TIME-CONSTANT DEFINER ROUTINES

(defclass load-time-constant-definer (variable-definer)
  ((prefix :accessor prefix-of :initform "%"))
  (:metaclass definer-class))

(defmethod initialize-definer ((definer load-time-constant-definer))
  (initialize-and-validate-variable-like-definer
   definer
   (list
    :documentation (make-option-writer documentation-of ensure-string-option)
    :prefix (make-option-writer prefix-of ensure-string-option))))

(defmethod expand-definer ((definer load-time-constant-definer))
  (let* ((name (name-of definer))
         (variable-name
          (intern (format nil "~:@(~s~s~)" (prefix-of definer) name))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defvar ,variable-name)
         (setf (documentation ',variable-name 'variable)
               ,(documentation-of definer))
         (unless (boundp ',variable-name)
           (setf ,variable-name ,(value-of definer))))
       (define-symbol-macro ,name (load-time-value ,variable-name))
       ,@(when (has-option-p definer #\e) `((export ',name))))))


;;; SPECIAL-VARIABLE DEFINER ROUTINES

(defclass special-variable-definer (variable-definer) ()
  (:metaclass definer-class))

(defmethod initialize-definer ((definer special-variable-definer))
  (initialize-variable-like-definer
   definer
   (combine-option-writers
    (list
     :documentation (make-option-writer documentation-of ensure-string-option)))))

(defmethod expand-definer ((definer special-variable-definer))
  (let ((name (name-of definer))
        (documentation (documentation-of definer)))
    `(progn
       (defvar ,name)
       ,@(when documentation
           `((setf (documentation ',name 'variable) ,documentation)))
       (makunbound ',name)
       ,@(when (slot-boundp definer 'value)
           `((setf ,name ,(value-of definer))))
       ,@(when (has-option-p definer #\e) `((export ',name))))))


;;; SYMBOL-MACRO DEFINER ROUTINES

(defclass symbol-macro-definer (variable-definer) ()
  (:metaclass definer-class))

(defmethod initialize-definer ((definer variable-definer))
  (initialize-and-validate-variable-like-definer definer))

(defmethod expand-definer ((definer symbol-macro-definer))
  (let ((name (name-of definer)))
    `(progn
       (define-symbol-macro ,name ,(value-of definer))
       ,@(when (has-option-p definer #\e) `((export ',name))))))
