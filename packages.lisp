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
  (:export #:block))

(define-package common-lisp/function
  (:nicknames cl/function function)
  (:recycle cl)
  (:export #:+
           #:-
           #:/
           #:*
           #:=
           #:/=
           #:<
           #:>
           #:<=
           #:>=
           #:abort
           #:abs
           #:acons
           #:acos
           #:acosh
           #:adjoin
           #:adjust-array
           #:adjustable-array-p
           #:alpha-char-p
           #:alphanumericp
           #:append
           #:apply
           #:apropos
           #:apropos-list
           #:arithmetic-error-operands
           #:arithmetic-error-operation
           #:array-dimension
           #:array-dimensions
           #:array-displacement
           #:array-element-type
           #:array-has-fill-pointer-p
           #:array-in-bounds-p
           #:array-rank
           #:array-row-major-index
           #:array-total-size
           #:arrayp
           #:ash
           #:asin
           #:asinh
           #:assoc
           #:assoc-if
           #:assoc-if-not
           #:atan
           #:atanh
           #:atom
           #:bit-and
           #:bit-andc1
           #:bit-andc2
           #:bit-eqv
           #:bit-ior
           #:bit-nand
           #:bit-nor
           #:bit-not
           #:bit-orc1
           #:bit-orc2
           #:bit-xor
           #:bit-vector-p
           #:boole))

(define-package common-lisp/accessor-function
  (:nicknames cl/accessor-function accessor-function accessor)
  (:recycle cl)
  (:export #:aref
           #:bit))

(define-package common-lisp/macro-function
  (:nicknames cl/macro-function macro-function)
  (:recycle cl)
  (:export #:and
           #:assert))

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
  (:export #:add-method
           #:allocate-instance))

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
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/binding
  (:nicknames cl/binding binding)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/reader
  (:nicknames cl/reader reader)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/evaluator
  (:nicknames cl/evaluator evaluator)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/compiler
  (:nicknames cl/compiler compiler)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/package
  (:nicknames cl/package package)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/type
  (:nicknames cl/type type)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export #:atom
           #:base-char
           #:base-string
           #:bignum
           #:bit
           #:boolean))

(define-package common-lisp/class
  (:nicknames cl/class class)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export #:array
           #:bit-vector))

(define-package common-lisp/object
  (:nicknames cl/object object)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/structure
  (:nicknames cl/structure structure)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/condition
  (:nicknames cl/condition condition)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export #:arithmetic-error))

;; A valid Type-Specifier is the name of a defined type, class, struct, condition,
;; or one of the symbols below as the operator of a compound type specifier

(define-package common-lisp/type-specifier
  (:nicknames cl/type-specifier cl/typespec type-specifier typespec)
  (:reexport cl/generic-interface cl/type cl/class cl/struct cl/condition)
  (:recycle cl)
  (:export #:and))

(define-package common-lisp/iterator
  (:nicknames cl/iterator iterator)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/symbol
  (:nicknames cl/symbol symbol)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/number
  (:nicknames cl/number number)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/fixnum
  (:nicknames cl/fixnum fixnum)
  (:recycle cl)
  (:export))

(define-package common-lisp/constant/fixnum
  (:nicknames cl/constant/fixnum const/fixnum)
  (:recycle cl)
  (:export #:array-dimension-limit
           #:array-rank-limit
           #:array-total-size-limit))

(define-package common-lisp/integer
  (:nicknames cl/integer integer)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/real-number
  (:nicknames cl/real-number real-number)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/rational-number
  (:nicknames cl/rational-number rational-number)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/complex-number
  (:nicknames cl/complex-number complex-number)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/character
  (:nicknames cl/character character)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/cons-cells
  (:nicknames cl/cons-cells cons-cells)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/sequence
  (:nicknames cl/sequence)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/array
  (:nicknames cl/array array)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/vector
  (:nicknames cl/vector vector)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/string
  (:nicknames cl/string string)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/list
  (:nicknames cl/list list)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/association-list
  (:nicknames cl/association-list association-list)
  (:recycle cl)
  (:reexport cl/generic-interface)
  (:export))

(define-package common-lisp/property-list
  (:nicknames cl/property-list property-list)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/hash-table
  (:nicknames cl/hash-table hash-table)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/pathname
  (:nicknames cl/pathname pathname)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/file
  (:nicknames cl/file file)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/stream
  (:nicknames cl/stream stream)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/printer
  (:nicknames cl/printer printer)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/format-string
  (:nicknames cl/format-string format-string)
  (:reexport cl/generic-interface)
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
