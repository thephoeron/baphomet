;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :baphomet)

;;;; === [[ :: [function] SAFE-ENDP :: ]] ================================================================================================================= ;;;;

(declaim (inline safe-endp))
(defun safe-endp (x)
  (declare (optimize safety))
  (endp x))

;;;; === [[ :: [function] ENSURE-SYMBOL :: ]] ============================================================================================================ ;;;;

;; Liberated from Alexandria
(declaim (inline ensure-symbol))
(defun ensure-symbol (name &optional (package *package*))
  "=> PACKAGE-QUALIFIED-SYMBOL, EXTERNAL-VISIBILITY-TYPE

Returns a symbol with name designated by NAME, accessible in package
designated by PACKAGE. If symbol is not already accessible in PACKAGE, it is
interned there. Returns a secondary value reflecting the status of the symbol
in the package, which matches the secondary return value of INTERN.

Example:

```lisp
(ensure-symbol :cons :cl) => cl:cons, :external
```
"
  (intern (string name) package))

;;;; === [[ :: [function] ALIST-PLIST :: ]] =============================================================================================================== ;;;;

;; Liberated from Alexandria
(defun alist-plist (alist)
  "Returns a property list containing the same keys and values as the
association list ALIST in the same order."
  (let (plist)
    (dolist (pair alist)
      (push (car pair) plist)
      (push (cdr pair) plist))
    (nreverse plist)))

;;;; === [[ :: [function] PLIST-ALIST :: ]] =============================================================================================================== ;;;;

;; Liberated from Alexandria
(defun plist-alist (plist)
  "Returns an association list containing the same keys and values as the
property list PLIST in the same order."
  (let (alist)
    (do ((tail plist (cddr tail)))
        ((safe-endp tail) (nreverse alist))
      (push (cons (car tail) (cadr tail)) alist))))

;;;; === [[ :: [function] HASH-TABLE-ALIST :: ]] ========================================================================================================== ;;;;

;; Liberated from Alexandria
(defun hash-table-alist (table)
  "Returns an association list containing the keys and values of hash table
TABLE."
  (let ((alist nil))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    alist))

;;;; === [[ :: [function] HASH-TABLE-PLIST :: ]] ========================================================================================================== ;;;;

;; Liberated from Alexandria
(defun hash-table-plist (table)
  "Returns a property list containing the keys and values of hash table
TABLE."
  (let ((plist nil))
    (maphash (lambda (k v)
               (setf plist (list* k v plist)))
             table)
    plist))

;;;; === [[ :: [derived-type] STRING-DESIGNATOR :: ]] ===================================================================================================== ;;;;

#+lispworks
(deftype type-designator ()
  '(satisfies system::valid-type-specifier))

#-lispworks
(deftype type-designator ()
  `(satisfies type-specifier-p))

;;;; === [[ :: [derived-type] STRING-DESIGNATOR :: ]] ===================================================================================================== ;;;;

(defun string-designator-p (thing)
  (typep thing 'string-designator))

(deftype string-designator ()
  '(or string symbol character null))

;;;; === [[ :: [derived-type] PROPER-LIST :: ]] =========================================================================================================== ;;;;

(defun proper-list-p (thing &optional (element-type '*))
  (typep thing `(and list (proper-list ,element-type))))

(deftype proper-list (&optional (element-type '*))
  `(and list (cons ,element-type *)))

;;;; === [[ :: [derived-type] PROPERTY-LIST :: ]] ========================================================================================================= ;;;;

(defun property-list-p (thing)
  (typep thing 'property-list))

(deftype property-list (&optional (element-type '*))
  `(and list (cons keyword (cons ,element-type *))))

;;;; === [[ :: [derived-type] ASSOCIATION-LIST :: ]] ====================================================================================================== ;;;;

(defun association-list-p (thing &optional (key-type 'string-designator) (element-type '*))
  (typep thing `(association-list ,@key-type ,@element-type)))

(deftype association-list (&optional (key-type 'string-designator) (element-type '*))
  `(and list (cons (cons ,key-type ,element-type) *)))

;;;; === [[ :: [parametric-type] EVAL-OP :: ]] ================================================================================================ ;;;;

;; PS: this is really a sequent. Shhh don't tell anyone!
(deftype eval-op (typed-lambda-list consequence)
  `(function ,typed-lambda-list ,consequence))

(defmacro eval-op (op type-name typed-lambda-list consequence)
  `(declaim (ftype (,op ,typed-lambda-list ,consequence) ,type-name)))

;; left-fold: reduce form, left-to-right
(deftype -> (typed-lambda-list reduction-value)
  `(eval-op ,typed-lambda-list ,reduction-value))

(defmacro -> (type-name typed-lambda-list reduction-value)
  `(eval-op -> ,type-name ,typed-lambda-list ,reduction-value))

;; right-fold: reduce form, right-to-left
(deftype <- (typed-lambda-list reduction-value)
  `(eval-op ,typed-lambda-list ,reduction-value))

(defmacro <- (type-name typed-lambda-list reduction-value)
  `(eval-op <- ,type-name ,typed-lambda-list ,reduction-value))

;; expand form in-place destructively
(deftype >> (typed-lambda-list expansion-value)
  `(eval-op ,typed-lambda-list ,expansion-value))

(defmacro >> (type-name typed-lambda-list expansion-value)
  `(eval-op >> ,type-name ,typed-lambda-list ,expansion-value))

;; evaluate in lexical environment, then return to caller
(deftype => (typed-lambda-list return-value)
  `(eval-op ,typed-lambda-list ,return-value))

(defmacro => (type-name typed-lambda-list return-value)
  `(eval-op => ,type-name ,typed-lambda-list ,return-value))

;; serialized evaluation in dynamic environment, typically due to structure or for side-effects
(deftype *> (typed-lambda-list implicit-return-value)
  `(eval-op ,typed-lambda-list ,implicit-return-value))

(defmacro *> (type-name typed-lambda-list implicit-return-value)
  `(eval-op *> ,type-name ,typed-lambda-list ,implicit-return-value))

;; dynamic closure
(deftype <*> (bindings captured-environment)
  `(eval-op ,bindings ,captured-environment))

(defmacro <*> (type-name bindings captured-environment)
  `(eval-op <*> ,type-name ,bindings ,captured-environment))

;;;; === [[ :: [struct] MODULE :: ]] ====================================================================================================================== ;;;;

(defstruct (module)
  (unit nil :type function)
  (symspace nil :type hash-table))

;;;; === [[ :: [struct] SIGNATURE :: ]] =================================================================================================================== ;;;;

(defstruct (signature (:type list) :named (:conc-name signature-) (:copier nil)
                      (:predicate signature-p)
                      (:constructor signature (eval-op name typed-lambda-list return-type)))
  (eval-op nil :type symbol :read-only t)
  (name nil :type string-designator :read-only t)
  (typed-lambda-list nil :type proper-list :read-only t)
  (return-type nil :type type-designator :read-only t))

(deftype signature ()
  '(or list null))

;;;; === [[ :: [struct] INTERFACE :: ]] =================================================================================================================== ;;;;

(defstruct (interface (:type list) :named (:conc-name i-) (:copier nil)
                      (:constructor interface (name module signature)))
  (name nil :type string)
  (module nil :type module)
  (signature nil :type signature)
  (implementations nil :type (or hash-table null)))

;;;; === [[ :: [struct] IMPLEMENTATION :: ]] ============================================================================================================== ;;;;

(defstruct (implementation)
  (assertions)
  (interface)
  (machine-code))
