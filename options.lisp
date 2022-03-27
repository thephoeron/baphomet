;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :baphomet)

;; DEFINER OPTIONS

(defun ensure-boolean-option (definer keyword value)
  (unless (typep value 'boolean)
    (error "Expecting a boolean instead of ~s for ~s in definer ~s of type ~s."
           value keyword (name-of definer) (definer-type definer)))
  value)

(defun ensure-string-option (definer keyword value)
  (unless (stringp value)
    (error "Expecting a string instead of ~s for ~s in definer ~s of type ~s."
           value keyword (name-of definer) (definer-type definer)))
  value)

(defun ensure-function-option (definer keyword value)
  (declare (ignore definer keyword))
  (if (and (consp value) (member (car value) '(function lambda)))
      value
      `(fdefinition ,value)))

(defun oerror (fmt options definer &rest rest)
  (apply #'error fmt options (name-of definer)
         (definer-type definer) rest))

(defun validate-definer-options (definer &optional extra-options-writer)
  "Validates definer options of function like definers."
  (destructuring-bind (options extra-options)
      ;; Extract definer options.
      (let* ((options (options-of definer))
             (probable-options (coerce (symbol-name (car options)) 'list))
             (available-options (available-definer-options definer)))
        (if (set-difference
             probable-options available-options :test #'char-equal)
            (list nil options)
            (list probable-options (rest options))))
    ;; Check extra options.
    (when (and extra-options (not extra-options-writer))
      (oerror "Invalid definer options ~s in definer ~s of type ~s."
              extra-options definer))
    ;; Check restricted options.
    (dolist (restricted-combination (restricted-definer-options definer))
      (when (every (lambda (option) (member option options :test #'char-equal))
                   restricted-combination)
        (oerror "Ambiguous definer options ~s in definer ~s of type ~s. ~
                 (Cannot use ~s definer options at the same.)"
                options definer restricted-combination)))
    ;; Update validated slot values.
    (setf (options-of definer) options)
    (when extra-options-writer
      (funcall extra-options-writer definer extra-options))))

(defun combine-option-writers (option-writers)
  (lambda (definer options)
    ;; Apply each OPTION-WRITER-FUNCTION, if found appropriate keyword in the
    ;; options of related definer.
    (loop with no-value-p = (gensym)
          for (keyword writer-function) on option-writers by #'cddr
          for option = (getf options keyword no-value-p)
          unless (eql option no-value-p)
          do (progn
               (funcall writer-function definer keyword option)
               (remf options keyword)))
    ;; Check whether we processed all options.
    (unless (null options)
      (oerror "Invalid options ~s for definer ~s of type ~s."
              options definer))))

(defmacro make-option-writer (slot-writer &optional validator)
  (with-unique-names (definer keyword value)
    `(lambda (,definer ,keyword ,value)
       ,@(unless validator
           `((declare (ignore ,keyword))))
       (setf (,slot-writer ,definer)
             ,(if validator
                  `(,validator ,definer ,keyword ,value)
                  value)))))

(defun has-option-p (definer option)
  (member option (options-of definer) :test #'char-equal))
