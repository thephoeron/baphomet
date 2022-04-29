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

(define-package common-lisp/special-operator
  (:nicknames cl/special-operator special-operator spec-op specop)
  (:recycle cl)
  (:export #:block
           #:catch
           #:eval-when
           #:flet
           #:function
           #:go
           #:if
           #:labels
           #:let
           #:let*
           #:load-time-value
           #:locally
           #:macrolet
           #:multiple-value-call
           #:multiple-value-prog1
           #:progn
           #:progv
           #:quote
           #:return-from
           #:setq
           #:symbol-macrolet
           #:tagbody
           #:the
           #:throw
           #:unwind-protect))

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
           #:boole
           #:both-case-p
           #:boundp
           #:break
           #:broadcast-stream-streams
           #:butlast
           #:byte
           #:byte-position
           #:byte-size
           #:ceiling
           #:cell-error-name
           #:cerror
           #:char-code
           #:char-downcase
           #:char-equal
           #:char-greaterp
           #:char-int
           #:char-lessp
           #:char-name
           #:char-not-equal
           #:char-not-greaterp
           #:char-not-lessp
           #:char-upcase
           #:char/=
           #:char<
           #:char<=
           #:char=
           #:char>
           #:char>=
           #:character
           #:characterp
           #:cis
           #:class-of
           #:clear-input
           #:clear-output
           #:close
           #:clrhash
           #:code-char
           #:coerce
           #:compile
           #:compile-file
           #:compile-file-pathname
           #:compiled-function-p
           #:complement
           #:complex
           #:complexp
           #:compute-restarts
           #:concatenate
           #:concatenated-stream-streams
           #:conjugate
           #:cons
           #:consp
           #:constantly
           #:constantp
           #:continue
           #:copy-alist
           #:copy-list
           #:copy-pprint-dispatch
           #:copy-readtable
           #:copy-seq
           #:copy-structure
           #:copy-symbol
           #:copy-tree
           #:cos
           #:cosh
           #:count
           #:count-if
           #:count-if-not
           #:decode-float
           #:decode-universal-time
           #:delete
           #:delete-duplicates
           #:delete-file
           #:delete-if
           #:delete-if-not
           #:delete-package
           #:denominator
           #:deposit-field
           #:describe
           #:digit-char
           #:digit-char-p
           #:directory
           #:directory-namestring
           #:disassemble
           #:dpb
           #:dribble
           #:echo-stream-input-stream
           #:echo-stream-output-stream
           #:ed
           #:encode-universal-time
           #:endp
           #:enough-namestring
           #:ensure-directories-exist
           #:ensure-generic-function
           #:eq
           #:eql
           #:equal
           #:equalp
           #:error
           #:eval
           #:evenp
           #:every
           #:exp
           #:expt
           #:export
           #:fboundp
           #:fceiling
           #:ffloor
           #:file-author
           #:file-error-pathname
           #:file-length
           #:file-namestring
           #:file-position
           #:file-string-length
           #:file-write-date
           #:fill
           #:find
           #:find-if
           #:find-if-not
           #:find-all-symbols
           #:find-package
           #:find-restart
           #:find-symbol
           #:finish-output
           #:float
           #:float-digits
           #:float-precision
           #:float-radix
           #:float-sign
           #:floatp
           #:floor
           #:fmakunbound
           #:force-output
           #:format
           #:fresh-line
           #:fround
           #:ftruncate
           #:funcall
           #:function-lambda-expression
           #:functionp
           #:gcd
           #:gensym
           #:gentemp
           #:get-decoded-time
           #:get-dispatch-macro-character
           #:get-internal-real-time
           #:get-internal-run-time
           #:get-macro-character
           #:get-output-stream-string
           #:get-properties
           #:get-setf-expansion
           #:get-universal-time
           #:graphic-char-p
           #:hash-table-count
           #:hash-table-p
           #:hash-table-rehash-size
           #:hash-table-rehash-threshold
           #:hash-table-size
           #:hash-table-test
           #:host-namestring
           #:identity
           #:imagpart
           #:import
           #:input-stream-p
           #:inspect
           #:integer-decode-float
           #:integer-length
           #:integerp
           #:interactive-stream-p
           #:intern
           #:intersection
           #:invalid-method-error
           #:invoke-debugger
           #:invoke-restart
           #:invoke-restart-interactively
           #:isqrt
           #:keywordp
           #:last
           #:lcm
           #:ldb-test
           #:ldiff
           #:length
           #:lisp-implementation-type
           #:lisp-implementation-version
           #:list
           #:list*
           #:list-all-packages
           #:list-length
           #:listen
           #:listp
           #:load
           #:load-logical-pathname-translations
           #:log
           #:logand
           #:logandc1
           #:logandc2
           #:logeqv
           #:logior
           #:lognand
           #:lognor
           #:lognot
           #:logorc1
           #:logorc2
           #:logxor
           #:logtest
           #:logbitp
           #:logcount
           #:logical-pathname
           #:long-site-name
           #:lower-case-p
           #:machine-instance
           #:machine-type
           #:machine-version
           #:macroexpand
           #:macroexpand-1
           #:make-array
           #:make-broadcast-stream
           #:make-concatenated-stream
           #:make-condition
           #:make-dispatch-macro-character
           #:make-echo-stream
           #:make-hash-table
           #:make-list
           #:make-load-form-saving-slots
           #:make-package
           #:make-pathname
           #:make-random-state
           #:make-sequence
           #:make-string
           #:make-string-input-stream
           #:make-string-output-stream
           #:make-symbol
           #:make-synonym-stream
           #:make-two-way-stream
           #:makunbound
           #:map
           #:map-into
           #:mapc
           #:mapcar
           #:mapcan
           #:mapl
           #:maplist
           #:mapcon
           #:maphash
           #:max
           #:member
           #:member-if
           #:member-if-not
           #:merge
           #:merge-pathnames
           #:method-combination-error
           #:min
           #:minusp
           #:mismatch
           #:mod
           #:muffle-warning
           #:name-char
           #:namestring
           #:nbutlast
           #:nconc
           #:nintersection
           #:not
           #:notany
           #:notevery
           #:nreconc
           #:nreverse
           #:nset-difference
           #:nset-exclusive-or
           #:nstring-capitalize
           #:nstring-downcase
           #:nstring-upcase
           #:nsublis
           #:nsubst
           #:nsubst-if
           #:nsubst-if-not
           #:nsubstitute
           #:nsubstitute-if
           #:nsubstitute-if-not
           #:nthcdr
           #:null
           #:numberp
           #:numerator
           #:nunion
           #:oddp
           #:open
           #:open-stream-p
           #:output-stream-p
           #:package-error-package
           #:package-name
           #:package-nicknames
           #:package-shadowing-symbols
           #:package-use-list
           #:package-used-by-list
           #:packagep
           #:pairlis
           #:parse-integer
           #:parse-namestring
           #:pathname
           #:pathname-device
           #:pathname-directory
           #:pathname-host
           #:pathname-match-p
           #:pathname-name
           #:pathname-type
           #:pathname-version
           #:pathnamep
           #:peek-char
           #:phase
           #:plusp
           #:position
           #:position-if
           #:position-if-not
           #:pprint
           #:pprint-dispatch
           #:pprint-fill
           #:pprint-indent
           #:pprint-linear
           #:pprint-newline
           #:pprint-tab
           #:pprint-tabular
           #:prin1
           #:prin1-to-string
           #:princ
           #:princ-to-string
           #:print
           #:print-not-readable-object
           #:probe-file
           #:proclaim
           #:provide
           #:random
           #:random-state-p
           #:rassoc
           #:rassoc-if
           #:rassoc-if-not
           #:rational
           #:rationalize
           #:rationalp
           #:read
           #:read-byte
           #:read-char
           #:read-char-no-hang
           #:read-delimited-list
           #:read-from-string
           #:read-line
           #:read-preserving-whitespace
           #:read-sequence
           #:readtablep
           #:realp
           #:realpart
           #:reduce
           #:rem
           #:remhash
           #:remove
           #:remove-if
           #:remove-if-not
           #:remove-duplicates
           #:remprop
           #:rename-file
           #:rename-package
           #:replace
           #:require
           #:restart-name
           #:revappend
           #:reverse
           #:room
           #:round
           #:rplaca
           #:rplacd
           #:scale-float
           #:search
           #:set
           #:set-difference
           #:set-dispatch-macro-character
           #:set-exclusive-or
           #:set-macro-character
           #:set-pprint-dispatch
           #:set-syntax-from-char
           #:shadow
           #:shadowing-import
           #:short-site-name
           #:signal
           #:signum
           #:simple-bit-vector-p
           #:simple-condition-format-arguments
           #:simple-condition-format-control
           #:simple-string-p
           #:simple-vector-p
           #:sin
           #:sinh
           #:sleep
           #:slot-boundp
           #:slot-exists-p
           #:slot-makunbound
           #:slot-value
           #:software-type
           #:software-version
           #:some
           #:sort
           #:special-operator-p
           #:sqrt
           #:stable-sort
           #:standard-char-p
           #:store-value
           #:stream-element-type
           #:stream-error-stream
           #:stream-external-format
           #:streamp
           #:string
           #:string-capitalize
           #:string-downcase
           #:string-equal
           #:string-greaterp
           #:string-left-trim
           #:string-lessp
           #:string-not-equal
           #:string-not-greaterp
           #:string-not-lessp
           #:string-right-trim
           #:string-trim
           #:string-upcase
           #:string/=
           #:string<
           #:string<=
           #:string=
           #:string>
           #:string>=
           #:stringp
           #:sublis
           #:subsetp
           #:subst
           #:subst-if
           #:subst-if-not
           #:substitute
           #:substitute-if
           #:substitute-if-not
           #:subtypep
           #:sxhash
           #:symbol-name
           #:symbol-package
           #:symbolp
           #:synonym-stream-symbol
           #:tailp
           #:tan
           #:tanh
           #:terpri
           #:translate-logical-pathname
           #:translate-pathname
           #:tree-equal
           #:truename
           #:truncate
           #:two-way-stream-input-stream
           #:two-way-stream-output-stream
           #:type-error-datum
           #:type-error-expected-type
           #:type-of
           #:typep))

(define-package common-lisp/accessor-function
  (:nicknames cl/accessor-function accessor-function accessor)
  (:recycle cl)
  (:export #:aref
           #:bit
           #:car
           #:cdr
           #:caar
           #:cadr
           #:cdar
           #:cddr
           #:caaar
           #:caadr
           #:cadar
           #:caddr
           #:cdaar
           #:cdadr
           #:cddar
           #:cdddr
           #:caaaar
           #:caaadr
           #:caadar
           #:caaddr
           #:cadaar
           #:cadadr
           #:caddar
           #:cadddr
           #:cdaaar
           #:cdaadr
           #:cdadar
           #:cdaddr
           #:cddaar
           #:cddadr
           #:cdddar
           #:cddddr
           #:char
           #:compiler-macro-function
           #:first
           #:second
           #:third
           #:fourth
           #:fifth
           #:sixth
           #:seventh
           #:eighth
           #:ninth
           #:tenth
           #:elt
           #:fdefinition
           #:fill-pointer
           #:find-class
           #:get
           #:getf
           #:gethash
           #:ldb
           #:logical-pathname-translations
           #:macro-function
           #:mask-field
           #:nth
           #:readtable-case
           #:rest
           #:row-major-aref
           #:sbit
           #:schar
           #:subseq
           #:svref
           #:symbol-function
           #:symbol-plist
           #:symbol-value))

(define-package common-lisp/local-function
  (:nicknames cl/local-function local-function)
  (:recycle cl)
  (:export #:call-next-method
           #:next-method-p))

(define-package common-lisp/macro-function
  (:nicknames cl/macro-function macro-function)
  (:recycle cl)
  (:export #:and
           #:assert
           #:case
           #:ccase
           #:ecase
           #:check-type
           #:cond
           #:ctypecase
           #:decf
           #:declaim
           #:defclass
           #:defconstant
           #:defgeneric
           #:define-compiler-macro
           #:define-condition
           #:define-method-combination
           #:define-modify-macro
           #:define-setf-expander
           #:define-symbol-macro
           #:defmacro
           #:defmethod
           #:defpackage
           #:defparameter
           #:defsetf
           #:defstruct
           #:deftype
           #:defun
           #:defvar
           #:destructuring-bind
           #:do
           #:do*
           #:do-all-symbols
           #:do-external-symbols
           #:do-symbols
           #:dolist
           #:dotimes
           #:etypecase
           #:formatter
           #:handler-bind
           #:handler-case
           #:ignore-errors
           #:in-package
           #:incf
           #:lambda
           #:loop
           #:multiple-value-bind
           #:multiple-value-list
           #:multiple-value-setq
           #:nth-value
           #:or
           #:pop
           #:pprint-logical-block
           #:print-unreadable-object
           #:prog
           #:prog*
           #:prog1
           #:prog2
           #:psetf
           #:psetq
           #:push
           #:pushnew
           #:remf
           #:restart-bind
           #:restart-case
           #:return
           #:rotatef
           #:setf
           #:shiftf
           #:step
           #:time
           #:trace
           #:typecase
           #:untrace))

(define-package common-lisp/local-macro
  (:nicknames cl/local-macro local-macro)
  (:recycle cl)
  (:export #:call-method
           #:make-method
           #:loop-finish
           #:pprint-exit-if-list-exhausted
           #:pprint-pop))

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
           #:allocate-instance
           #:change-class
           #:class-name
           #:compute-applicable-methods
           #:describe-object
           #:documentation
           #:find-method
           #:function-keywords
           #:initialize-instance
           #:make-instance
           #:make-instances-obsolete
           #:make-load-form
           #:method-qualifiers
           #:no-applicable-method
           #:no-next-method
           #:print-object
           #:reinitialize-instance
           #:remove-method
           #:shared-initialize
           #:slot-missing
           #:slot-unbound))

(define-package common-lisp/method
  (:nicknames cl/method method)
  (:reexport cl/generic-function)
  (:recycle cl)
  (:export))

;; Declarations can be directly re-exported too

(define-package common-lisp/declaration
  (:nicknames cl/declaration declaration)
  (:recycle cl)
  (:export #:declaration
           #:dynamic-extent
           #:ftype
           #:ignorable
           #:ignore
           #:inline
           #:notinline
           #:optimize
           #:special
           #:type))

(define-package common-lisp/declaration/optimize/quality
  (:nicknames cl/decl/optimize/quality optimize-quality quality)
  (:recycle cl)
  (:export #:compilation-speed
           #:debug
           #:safety
           #:space
           #:speed))

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
           #:boolean
           #:compiled-function
           #:double-float
           #:extended-char
           #:fixnum
           #:keyword
           #:long-float
           #:nil
           #:short-float
           #:single-float
           #:signed-byte
           #:simple-array
           #:simple-base-string
           #:simple-bit-vector
           #:simple-string
           #:simple-vector
           #:standard-char))

(define-package common-lisp/class
  (:nicknames cl/class class)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export #:array
           #:bit-vector
           #:broadcast-stream
           #:built-in-class
           #:character
           #:class
           #:complex
           #:concatenated-stream
           #:cons
           #:echo-stream
           #:file-stream
           #:float
           #:function
           #:generic-function
           #:hash-table
           #:integer
           #:list
           #:logical-pathname
           #:method
           #:method-combination
           #:null
           #:number
           #:package
           #:pathname
           #:random-state
           #:ratio
           #:rational
           #:readtable
           #:real
           #:restart
           #:sequence
           #:standard-class
           #:standard-generic-function
           #:standard-method
           #:standard-object
           #:stream
           #:string
           #:string-stream
           #:structure-class
           #:structure-object
           #:symbol
           #:synonym-stream
           #:t
           #:two-way-stream))

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
  (:export #:arithmetic-error
           #:cell-error
           #:condition
           #:control-error
           #:division-by-zero
           #:end-of-file
           #:error
           #:file-error
           #:floating-point-inexact
           #:floating-point-invalid-operation
           #:floating-point-overflow
           #:floating-point-underflow
           #:package-error
           #:parse-error
           #:print-not-readable
           #:program-error
           #:reader-error
           #:serious-condition
           #:simple-condition
           #:simple-error
           #:simple-type-error
           #:simple-warning
           #:storage-condition
           #:stream-error
           #:style-warning
           #:type-error))

(define-package common-lisp/restart
  (:nicknames cl/restart restart)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export #:abort
           #:continue
           #:muffle-warning
           #:store-value
           #:use-value))

;; A valid Type-Specifier is the name of a defined type, class, struct, condition,
;; or one of the symbols below as the operator of a compound type specifier

(define-package common-lisp/type-specifier
  (:nicknames cl/type-specifier cl/typespec type-specifier typespec)
  (:reexport cl/generic-interface cl/type cl/class cl/struct cl/condition)
  (:recycle cl)
  (:export #:and
           #:eql
           #:member
           #:mod
           #:not
           #:or
           #:satisfies))

(define-package common-lisp/boolean
  (:nicknames cl/boolean boolean)
  (:recycle cl)
  (:export #:nil
           #:t))

(define-package common-lisp/symbol
  (:nicknames cl/symbol symbol)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export #:declare
           #:function
           #:lambda
           #:method-combination
           #:otherwise
           #:setf
           #:structure
           #:t
           #:type))

(define-package common-lisp/number
  (:nicknames cl/number number)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/fixnum
  (:nicknames cl/fixnum fixnum)
  (:reexport cl/generic-interface)
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

(define-package common-lisp/constant/integer
  (:nicknames cl/constant/integer const/integer)
  (:recycle cl)
  (:export #:call-arguments-limit
           #:char-code-limit
           #:internal-time-units-per-second
           #:lambda-parameters-limit
           #:multiple-values-limit))

;; Will need to create type-packages for double, long, short, and single floats,
;; and reexport them in cl/float, and likewise for cl/constant/float. Ech.

(define-package common-lisp/float
  (:nicknames cl/float float)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export))

(define-package common-lisp/constant/float
  (:nicknames cl/constant/float const/float)
  (:recycle cl)
  (:export #:double-float-epsilon
           #:double-float-negative-epsilon
           #:long-float-epsilon
           #:long-float-negative-epsilon
           #:short-float-epsilon
           #:short-float-negative-epsilon
           #:single-float-epsilon
           #:single-float-negative-epsilon
           #:least-negative-double-float
           #:least-negative-long-float
           #:least-negative-normalized-double-float
           #:least-negative-normalized-long-float
           #:least-negative-normalized-short-float
           #:least-negative-normalized-single-float
           #:least-negative-short-float
           #:least-negative-single-float
           #:least-positive-double-float
           #:least-positive-long-float
           #:least-positive-normalized-double-float
           #:least-positive-normalized-long-float
           #:least-positive-normalized-short-float
           #:least-positive-normalized-single-float
           #:least-positive-short-float
           #:least-positive-single-float
           #:most-negative-double-float
           #:most-negative-fixnum
           #:most-negative-long-float
           #:most-negative-short-float
           #:most-negative-single-float
           #:most-positive-double-float
           #:most-positive-fixnum
           #:most-positive-long-float
           #:most-positive-short-float
           #:most-positive-single-float
           #:pi))

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
  (:export #:nil))

(define-package common-lisp/constant/list
  (:nicknames cl/constant/list const/list)
  (:reexport cl/generic-interface)
  (:recycle cl)
  (:export #:lambda-list-keywords))

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
