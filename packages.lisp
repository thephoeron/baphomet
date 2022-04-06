;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :baphomet/sys)

;; first up, we take care of some implementation-dependent issues

#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *packages-for-warn-on-redefinition* nil
        *handle-warn-on-redefinition* :quiet)
  (rename-package 'dspec 'dspec '(defspec defspecs definition-specs definition-specifications)))

;; Then we define packages for the Common Lisp object/type model, and re-export
;; symbols according to the packages-as-types convention

;; Funcallables are directly re-exported

(define-package common-lisp/special-form
  (:nicknames cl/special-form special-form)
  (:recycle cl)
  (:export))

(define-package common-lisp/function
  (:nicknames cl/function function)
  (:recycle cl)
  (:export))

(define-package common-lisp/macro-function
  (:nicknames cl/macro-function macro-function)
  (:recycle cl)
  (:export))

(define-package common-lisp/compiler-macro
  (:nicknames cl/compiler-macro compiler-macro)
  (:recycle cl)
  (:export))

(define-package common-lisp/symbol-macro
  (:nicknames cl/symbol-macro symbol-macro)
  (:recycle cl)
  (:export))

(define-package common-lisp/reader-macro
  (:nicknames cl/reader-macro reader-macro)
  (:recycle cl)
  (:export))

(define-package common-lisp/modify-macro
  (:nicknames cl/modify-macro modify-macro)
  (:recycle cl)
  (:export))

(define-package common-lisp/fexpr
  (:nicknames cl/fexpr fexpr)
  (:recycle cl)
  (:export))

(define-package common-lisp/generic-function
  (:nicknames cl/generic-function generic-function)
  (:recycle cl)
  (:export))

(define-package common-lisp/method
  (:nicknames cl/method method)
  (:recycle cl)
  (:export))

;; Other objects/types share a generic interface

(define-package common-lisp/generic-interface
  (:nicknames cl/generic-interface generic-interface)
  (:use c2cl)
  (:export #:instancep
           #:new
           #:redefine
           #:copy
           #:initialize
           #:collapse
           #:invoke))

(define-package common-lisp/environment
  (:nicknames cl/environment environment)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/binding
  (:nicknames cl/binding binding)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/reader
  (:nicknames cl/reader reader)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/evaluator
  (:nicknames cl/evaluator evaluator)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/compiler
  (:nicknames cl/compiler compiler)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/package
  (:nicknames cl/package package)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/type
  (:nicknames cl/type type)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/class
  (:nicknames cl/class class)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/object
  (:nicknames cl/object object)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/iterator
  (:nicknames cl/iterator iterator)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/structure
  (:nicknames cl/structure structure)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/condition
  (:nicknames cl/condition condition)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/symbol
  (:nicknames cl/symbol symbol)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/number
  (:nicknames cl/number number)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/integer
  (:nicknames cl/integer integer)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/real-number
  (:nicknames cl/real-number real-number)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/rational-number
  (:nicknames cl/rational-number rational-number)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/complex-number
  (:nicknames cl/complex-number complex-number)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/character
  (:nicknames cl/character character)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/cons-cells
  (:nicknames cl/cons-cells cons-cells)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/sequence
  (:nicknames cl/sequence)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/array
  (:nicknames cl/array array)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/vector
  (:nicknames cl/vector vector)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/string
  (:nicknames cl/string string)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/list
  (:nicknames cl/list list)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/association-list
  (:nicknames cl/association-list association-list)
  (:recycle cl)
  (:use cl/generic-interface)
  (:export))

(define-package common-lisp/property-list
  (:nicknames cl/property-list property-list)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/hash-table
  (:nicknames cl/hash-table hash-table)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/pathname
  (:nicknames cl/pathname pathname)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/file
  (:nicknames cl/file file)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/stream
  (:nicknames cl/stream stream)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/printer
  (:nicknames cl/printer printer)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/format-string
  (:nicknames cl/format-string format-string)
  (:use cl/generic-interface)
  (:recycle cl)
  (:export))

;; Now we can define the Baphomet package to handle the machinery of extensible
;; definers

(define-package baphomet
  (:use c2cl)
  (:documentation "Extensible definer macros.")
  #+lispworks
  (:shadowing-import-from lispworks
           #:with-unique-names
           #:*user-package*)
  (:import-from serapeum
           #:dict
           #:href
           #:@)
  (:import-from trivial-types
           #:type-specifier-p)
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
           ;; Evaluation Operators
           #:eval-op
           #:->
           #:<-
           #:>>
           #:=>
           #:*>
           #:<*>
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

(define-package def
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
