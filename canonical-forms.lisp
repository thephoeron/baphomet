;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :baphomet)

(defmacro when-let ((var test) &body body)
  `(let ((,var ,test))
     (when ,var ,@body)))

(defun ensure-function (form)
  (cond ((and (consp form) (member (car form) '(function quote))) (second form))
        ((or (atom form) (and (consp form) (eql (car form) 'lambda))) form)
        (t (error "Invalid ENSURE-FUNCTION input: ~A" form))))

;;; Copied from lists.lisp of alexandria project. (See
;;; http://common-lisp.net/project/alexandria/)
(defun ensure-list (form)
  "If FORM is a list, it is returned. Otherwise returns the list designated by
FORM."
  (if (listp form)
      form
      (list form)))

;;; ENSURE-PLIST

(defun ensure-plist (form)
  "If FORM is of type PROPERTY-LIST, return it. Otherwise, attempt to
   coerce FORM to an object of type PROPERTY-LIST or fail miserably."
    (typecase form
      (null nil)
      (property-list form)
      (association-list (alist-plist form))
      (hash-table (hash-table-plist form))
      (proper-list (ensure-plist (append (car form) (ensure-plist (cdr form)))))
      (otherwise (error "The ~A parameter value cannot be coerced to a PLIST" (type-of form)))))

(defun assoc-val (item alist)
  (let ((val (assoc item alist)))
    (typecase val
      (null nil)
      (cons (or (cadr val) :no-value))
      (otherwise (error "You done effed up.")))))

(defun assoc-val-if (predicate alist &key key))

(defun assoc-val-if-not (predicate alist &key key))

;;; Copied from cl-ppcre project of Dr. Edmund Weitz. Reference implementation
;;; posted to comp.lang.lisp as <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa.
#-lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))

(defun parse-components (body)
  "Parses supplied BODY with respect to ([[declaration* | documentation]] form*)
pattern and returns list of DECLARATIONS, DOCUMENTATION and FORM values."
  (loop with declarations
        with documentation
        for forms on body
        for form = (first forms)
        while forms
        do (let ((form (car forms)))
             (cond ((and (listp form)
                         (eql (car form) 'declare))
                    (push form declarations))
                   ((and (stringp form)
                         (null documentation)
                         (cdr forms))
                    (setq documentation form))
                   (t (loop-finish))))
        finally (return (list (nreverse declarations) documentation forms))))

(defmacro with-components ((declarations docstring rest) target-body &body body)
  "Binds passed DECLARATIONS, DOCSTRING and REST to the related parts of the
TARGET-BODY."
  `(destructuring-bind (,declarations ,docstring ,rest)
       (parse-components ,target-body)
     ,@body))

;; rename to def:print-object
(defmacro def:print-object-method ((class &key (identity t) (type t) package)
                                   (self &optional stream) &body body)
  "Define a PRINT-OBJECT method using PRINT-UNREADABLE-OBJECT."
  (let ((stream-symbol (or stream (gensym))))
    (with-components (declarations docstring body) body
      `(defmethod print-object ((,self ,class) ,stream-symbol)
         ,@declarations
         ,@(when docstring (list docstring))
         (print-unreadable-object
             (,self ,stream-symbol :type ,type :identity ,identity)
           (let (,@(unless stream `((*standard-output* ,stream-symbol)))
                 ,@(when package `((*package ,(find-package package)))))
             ,@body))))))

;; Liberated from SHRDLU (the Common Lisp version in the Arcanum Collection)
(defmacro def:fexpr (fexpr-name (fexpr-arg) &body fexpr-body)
  "Improved (*i.e.*, working) way to implement the defun-fexpr"
  (let ((subr-name (intern (format nil "APPLY-~A" fexpr-name))))
    `(progn (defmacro ,fexpr-name (&rest ,fexpr-arg)
             `(,',subr-name ',,fexpr-arg))
            (defun ,subr-name (,fexpr-arg)
              ,@fexpr-body)
            ',fexpr-name)))
