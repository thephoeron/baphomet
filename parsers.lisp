;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :baphomet)

;; Update to use BAPHOMET definer syntax

;; Global DICT for all parsers
(defparameter *parsers* (dict :common-lisp nil))

;; Current Dynamic Parser Context
(defparameter *current-parser-context* :default)

;; IN-PARSER-CONTEXT macro
(defmacro in-parser-context (name)
  (let ((name-key (keywordize name)))
    `(setf *current-parser-context* ,name-key
           *parsers* ,name-key)))

;; DEFRULE MACRO
(defmacro defrule (name pattern &body body)
  "Nimrod macro to shut up compiler."
  `(push ,(href *parsers* *current-parser-context*)
          (list ,name ,pattern ,@body)))
