;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :cl-user)

(defpackage baphomet/asdf
  (:nicknames baphomet/sys)
  (:use cl asdf uiop))

(in-package :baphomet/asdf)

(defsystem baphomet
  :name "BAPHOMET"
  :description "Extensible, declarative, type-dispatched definition forms with consistent syntax."
  :author '("\"the Phoeron\" Colin J.E. Lupton <thephoeron@protonmail.com>"
            "Volkan Yazici <volkan.yazici@gmail.com>")
  :homepage "https://thephoeron.github.io/baphomet/"
  :source-control (:git "https://github.com/thephoeron/baphomet")
  :bug-tracker "https://github.com/thephoeron/baphomet/issues"
  :mailto "thephoeron@protonmail.com"
  :version (:read-file-form "VERSION")
  :licence "MIT"
  :depends-on (closer-mop
               serapeum
               alexandria
               trivial-types)
  :serial t
  :components ((:file "packages")
               (:module type-packages
                :serial t
                :components ((:file "generic-interface")))
               (:file "types")
               (:file "canonical-forms")
               (:file "parsers")
               (:file "definer")
               (:file "options")
               (:module definition-forms
                :serial t
                :components ((:file "bindings")
                             (:file "functions")
                             (:file "macros")
                             (:file "object-system"))))
  :perform (load-op :after (op c)
             (provide :baphomet)
             (pushnew :baphomet *features*)))

(defsystem baphomet/test
  :description "Test suite for BAPHOMET."
  :author "\"the Phoeron\" Colin J.E. Lupton <thephoeron@protonmail.com>"
  :homepage "https://thephoeron.github.io/baphomet/"
  :source-control (:git "https://github.com/thephoeron/baphomet")
  :bug-tracker "https://github.com/thephoeron/baphomet/issues"
  :mailto "thephoeron@protonmail.com"
  :version (:read-file-form "VERSION")
  :licence "MIT"
  :depends-on (baphomet
               parachute)
  :serial t
  :components ((:module test
                :components ((:file "suite")))))
