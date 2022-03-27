;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :cl-user)

;; first up, we take care of some implementation-dependent issues

#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *packages-for-warn-on-redefinition* nil
        *handle-warn-on-redefinition* :quiet)
  (rename-package 'dspec 'dspec '(defspec defspecs definition-specs definition-specifications)))

;; Then we define packages for the Common Lisp object/type model, and re-export
;; symbols according to the packages-as-types convention

(defpackage common-lisp/environment
  (:nicknames cl/environment environment)
  (:import-from cl)
  (:export))

(defpackage common-lisp/binding
  (:nicknames cl/binding binding)
  (:import-from cl)
  (:export))

(defpackage common-lisp/reader
  (:nicknames cl/reader reader)
  (:import-from cl)
  (:export))

(defpackage common-lisp/evaluator
  (:nicknames cl/evaluator evaluator)
  (:import-from cl)
  (:export))

(defpackage common-lisp/compiler
  (:nicknames cl/compiler compiler)
  (:import-from cl)
  (:export))

(defpackage common-lisp/package
  (:nicknames cl/package package)
  (:import-from cl)
  (:export))

(defpackage common-lisp/type
  (:nicknames cl/type type)
  (:import-from cl)
  (:export))

(defpackage common-lisp/class
  (:nicknames cl/class class)
  (:import-from cl)
  (:export))

(defpackage common-lisp/object
  (:nicknames cl/object object)
  (:import-from cl)
  (:export))

(defpackage common-lisp/iterator
  (:nicknames cl/iterator iterator)
  (:import-from cl)
  (:export))

(defpackage common-lisp/structure
  (:nicknames cl/structure structure)
  (:import-from cl)
  (:export))

(defpackage common-lisp/condition
  (:nicknames cl/condition condition)
  (:import-from cl)
  (:export))

(defpackage common-lisp/symbol
  (:nicknames cl/symbol symbol)
  (:import-from cl)
  (:export))

(defpackage common-lisp/number
  (:nicknames cl/number number)
  (:import-from cl)
  (:export))

(defpackage common-lisp/integer
  (:nicknames cl/integer integer)
  (:import-from cl)
  (:export))

(defpackage common-lisp/real-number
  (:nicknames cl/real-number real-number)
  (:import-from cl)
  (:export))

(defpackage common-lisp/rational-number
  (:nicknames cl/rational-number rational-number)
  (:import-from cl)
  (:export))

(defpackage common-lisp/complex-number
  (:nicknames cl/complex-number complex-number)
  (:import-from cl)
  (:export))

(defpackage common-lisp/character
  (:nicknames cl/character character)
  (:import-from cl)
  (:export))

(defpackage common-lisp/cons-cells
  (:nicknames cl/cons-cells cons-cells)
  (:import-from cl)
  (:export))

(defpackage common-lisp/sequence
  (:nicknames cl/sequence sequence)
  (:import-from cl)
  (:export))

(defpackage common-lisp/array
  (:nicknames cl/array array)
  (:import-from cl)
  (:export))

(defpackage common-lisp/vector
  (:nicknames cl/vector vector)
  (:import-from cl)
  (:export))

(defpackage common-lisp/string
  (:nicknames cl/string string)
  (:import-from cl)
  (:export))

(defpackage common-lisp/list
  (:nicknames cl/list list)
  (:import-from cl)
  (:export))

(defpackage common-lisp/association-list
  (:nicknames cl/association-list association-list)
  (:import-from cl)
  (:export))

(defpackage common-lisp/property-list
  (:nicknames cl/property-list property-list)
  (:import-from cl)
  (:export))

(defpackage common-lisp/hash-table
  (:nicknames cl/hash-table hash-table)
  (:import-from cl)
  (:export))

(defpackage common-lisp/pathname
  (:nicknames cl/pathname pathname)
  (:import-from cl)
  (:export))

(defpackage common-lisp/file
  (:nicknames cl/file file)
  (:import-from cl)
  (:export))

(defpackage common-lisp/stream
  (:nicknames cl/stream stream)
  (:import-from cl)
  (:export))

(defpackage common-lisp/printer
  (:nicknames cl/printer printer)
  (:import-from cl)
  (:export))

(defpackage common-lisp/format-string
  (:nicknames cl/format-string format-string)
  (:import-from cl)
  (:export))

;; Now we can define the Baphomet package to handle the machinery of extensible
;; definers

(defpackage baphomet
  (:use c2cl)
  (:documentation "Extensible definer macros.")
  #+lispworks
  (:shadowing-import-from lispworks
           #:with-unique-names)
  (:import-from #:serapeum
           #:dict
           #:href
           #:@)
  (:export ;; types
           #:string-designator-p
           #:string-designator
           #:proper-list-p
           #:proper-list
           #:property-list-p
           #:property-list
           #:association-list-p
           #:association-list
           #:module-p
           #:module
           #:signature-p
           #:signature
           #:interface-p
           #:interface
           #:implementation-p
           #:implementation
        ;; Utilities
           #:when-let
           #:ensure-function
           #:ensure-list
           #:ensure-plist
           #:with-components
        ;; Definers
           #:definer-class
           #:definer-name-of
           #:definer-options-of
           #:definer-signature-of
           #:definer-expansion-env-of
           #:definer-call-form-of
           #:definer-forms-of
           #:definer
           #:name-of
           #:options-of
           #:signature-of
           #:expansion-env-of
           #:call-form-of
           #:forms-of
           #:available-definer-options
           #:restricted-definer-options
           #:initialize-definer
           #:expand-definer
           #:definer-type
           #:keyword-definer
        ;; Definer Options
           #:ensure-boolean-option
           #:ensure-string-option
           #:ensure-function-option
           #:oerror
           #:validate-definer-options
           #:combine-option-writers
           #:make-option-writer
           #:has-option-p
        ;; Function Definers
           #:declare-optimize
           #:declare-debug
           #:initialize-function-like-definer
           #:expand-function-like-definer
           #:function-definer
           #:macro-definer
           #:compiler-macro-definer
           #:method-definer
           #:generic-definer
           #:type-definer
           #:print-object-definer
           #:setf-definer
        ;; Variable Definers
           #:variable-definer
           #:constant-definer
           #:load-time-constant-definer
           #:special-variable-definer
           #:symbol-macro-definer
        ;; Miscelaneous Definers
           #:extract-slots
           #:extract-class-accessors
           #:extract-struct-accessors
           #:ensure-slot-spec
           #:ensure-slot-spec-initargs
           #:ensure-slot-spec-accessors
           #:ensure-slot-spec-readers
           #:ensure-slot-spec-writers
           #:expand-class-like-definer
           #:class-definer
           #:condition-definer
           #:struct-definer))

;; Last but not least, we define the definer package to expose the API of
;; extensible definers

(defpackage def
  (:nicknames define)
  (:use c2cl baphomet)
  ;; base definition functor
  (:export #:definer)
  ;; models
  (:export #:sample
           #:model
           #:attribute
           #:property
           #:component
           #:assembly)
  ;; metamachines
  (:export #:state
           #:mode
           #:structure
           #:control)
  ;; machine models
  (:export #:machine
           #:literal
           #:address
           #:memory
           #:register
           #:cache
           #:closure
           #:continuation
           #:operator
           #:instruction
           #:pseudoinstruction
           #:label
           #:algorithm
           #:routine
           #:subroutine
           #:coroutine
           #:program)
  ;; domains / geometric spaces
  (:export #:map
           #:projection
           #:transformation
           #:mutation
           #:composition
           #:decomposition)
  ;; analogues
  (:export #:pattern
           #:placeholder
           #:modifier
           #:comparison
           #:difference
           #:similarity
           #:match)
  ;; category-theory
  (:export #:relation
           #:object
           #:collection
           #:ordering
           #:category
           #:functor)
  ;; Object Model of Common Lisp (plus Black Brane extensions)
  (:export #:object
           #:symbol
           #:package
           #:keyword
           #:primitive
           #:expression
           #:form
           #:lambda-form
           #:phi-form
           #:mu-form
           #:s-expression #:s-expr #:sexpr
           #:phi-expression #:phi-expr #:fexpr
           #:mu-expression #:mu-expr #:meta-expr #:mexpr
           #:application
           #:expansion
           #:substitution
           #:reduction
           #:evaluation
           #:reading
           #:compilation
           #:loading
           #:running
           #:variable
           #:constant
           #:operator
           #:special-operator
           #:symbol-macro
           #:binding
           #:lexical-binding
           #:dynamic-binding
           #:function-binding
           #:type-binding
           #:compiler-binding
           #:environment
           #:namespace
           #:sequence
           #:string
           #:vector
           #:array
           #:cons
           #:list
           #:property-list #:plist
           #:association-list #:alist
           #:lambda-list
           #:ordinary-lambda-list
           #:destructuring-lambda-list
           #:macro-lambda-list
           #:instance #:generator
           #:place #:constructor #:getter #:setter #:setf
           #:reader #:parser
           #:writer #:printer
           #:accessor
           #:predicate
           #:function
           #:compiler-macro
           #:macro-function
           #:generic-function
           #:template-function
           #:print-object-method
           #:method
           #:demon-method
           #:metaclass
           #:class
           #:slot
           #:struct
           #:type
           #:condition
           #:signal
           #:event))