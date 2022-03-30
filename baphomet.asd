;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :cl-user)

(defpackage baphomet/asdf
  (:use cl asdf uiop))

(in-package :baphomet/asdf)

(defsystem baphomet
  :name "BAPHOMET"
  :description "Extensible, declarative, type-dispatched definition forms with consistent syntax."
  :author '("\"the Phoeron\" Colin J.E. Lupton <thephoeron@protonmail.com>"
            "Volkan Yazici <volkan.yazici@gmail.com>")
  :homepage "https://thephoeron.github.io/baphomet/"
  :source-control "https://github.com/thephoeron/baphomet"
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
