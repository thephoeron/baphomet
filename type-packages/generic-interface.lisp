;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :common-lisp/generic-interface)

(defgeneric instancep (instance)
  (:documentation ""))

(defgeneric new (instance &rest args &key &allow-other-keys)
  (:documentation ""))

(defgeneric redefine (instance &rest args &key &allow-other-keys)
  (:documentation ""))

(defgeneric copy (instance &rest args &key &allow-other-keys)
  (:documentation ""))

(defgeneric initialize (instance &rest args &key &allow-other-keys)
  (:documentation ""))

(defgeneric collapse (instance &rest args &key &allow-other-keys)
  (:documentation ""))

(defgeneric invoke (instance &rest args &key &allow-other-keys)
  (:documentation ""))
