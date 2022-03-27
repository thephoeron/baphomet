;; Copyright (c) 2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;; Released under the MIT License. See baphomet/LICENSE for more information.

(in-package :baphomet)

;;; COMMON ROUTINES

(defun extract-slots (definer)
  (mapcar
   (lambda (spec) (car (ensure-list spec)))
   (funcall (ecase (class-name (class-of definer))
              ((class-definer condition-definer) #'slot-specs-of)
              (struct-definer #'slot-descs-of))
            definer)))

(defun ensure-slot-spec (definer test keyword fmt-accessor &optional (package *package*))
  (mapcar
   (lambda (slot-spec)
     (let ((slot-spec (ensure-list slot-spec)))
       (if (funcall test slot-spec)
           slot-spec
           (append
            slot-spec
            (list keyword
                  (intern
                   (format nil (string-upcase (funcall fmt-accessor definer))
                           (first slot-spec))
                   package))))))
   (slot-specs-of definer)))

(defun ensure-slot-spec-initargs (definer)
  (ensure-slot-spec
   definer
   (lambda (slot-spec) (getf (rest slot-spec) :initarg))
   :initarg #'initarg-format-of
   :keyword))

(defun ensure-slot-spec-accessors (definer)
  (ensure-slot-spec
   definer
   (lambda (slot-spec &aux (plist (rest slot-spec)))
     (or (getf plist :accessor)
         (and (getf plist :reader)
              (getf plist :writer))))
   :accessor #'accessor-format-of))

(defun ensure-slot-spec-readers (definer)
  (ensure-slot-spec
   definer
   (lambda (slot-spec) (getf (rest slot-spec) :reader))
   :reader #'reader-format-of))

(defun ensure-slot-spec-writers (definer)
  (ensure-slot-spec
   definer
   (lambda (slot-spec) (getf (rest slot-spec) :writer))
   :writer #'reader-format-of))

;; this is where the bad expansion is happening
(defun expand-class-like-definer (definer function)
  `(progn
     (,function ,(name-of definer) ,(superclasses-of definer)
       ,(slot-specs-of definer)
       ,@(class-options-of definer))
     ,@(when (has-option-p definer #\e) `((export ',(name-of definer))))
     ,@(when (has-option-p definer #\s) `((export ',(extract-slots definer))))
     ,@(when (has-option-p definer #\a)
         `((export ',(extract-class-accessors definer))))
     ,@(when (has-option-p definer #\m)
         (let ((make-sym (intern (format nil "~:@(make-~a~)" (name-of definer)))))
           (with-unique-names (keys)
             `((defun ,make-sym (&rest ,keys &key &allow-other-keys)
                 (apply #'make-instance ',(name-of definer) :allow-other-keys t ,keys))
               ,@(when (has-option-p definer #\e)
                   `((export ',make-sym)))))))))


;;; CLASS DEFINER ROUTINES

(defclass class-definer (definer)
  ((superclasses :accessor superclasses-of)
   (slot-specs :accessor slot-specs-of)
   (class-options :accessor class-options-of)
   (initarg-format :type string :initform "~s" :accessor initarg-format-of)
   (accessor-format :type string :initform "~s-of" :accessor accessor-format-of)
   (reader-format :type string :initform "~s-of" :accessor reader-format-of)
   (writer-format :type string :initform "~s-of" :accessor writer-format-of ))
  (:metaclass definer-class))

(defmethod available-definer-options ((definer class-definer))
  (list #\e #\a #\s #\n #\c #\r #\w #\m))

(defmethod restricted-definer-options ((definer class-definer))
  nil)

(defmethod initialize-definer ((definer class-definer))
  (destructuring-bind (name superclasses slot-specs &rest class-options)
      (forms-of definer)
    ;; Set (NAME-OF DEFINER) before any OERROR calls.
    (setf (name-of definer) name)
    (unless (listp slot-specs)
      (oerror "Expecting a slot-spec list in options ~s of definer ~
               ~s of type ~s."
              (options-of definer) definer))
    (setf (superclasses-of definer) superclasses
          (slot-specs-of definer) slot-specs
          (class-options-of definer) class-options))
  (validate-definer-options
   definer
   (combine-option-writers
    (list
     :initarg-format (make-option-writer
                      initarg-format-of ensure-string-option)
     :accessor-format (make-option-writer
                       accessor-format-of ensure-string-option)
     :reader-format (make-option-writer
                     reader-format-of ensure-string-option)
     :writer-format (make-option-writer
                     writer-format-of ensure-string-option))))
  (macrolet ((ensure-slot-spec-identifier (option ensure)
               `(when (has-option-p definer ,option)
                  (setf (slot-specs-of definer) (,ensure definer)))))
    (ensure-slot-spec-identifier #\n ensure-slot-spec-initargs)
    (ensure-slot-spec-identifier #\c ensure-slot-spec-accessors)
    (ensure-slot-spec-identifier #\r ensure-slot-spec-readers)
    (ensure-slot-spec-identifier #\w ensure-slot-spec-writers))
  definer)

(defun extract-class-accessors (definer)
  (labels ((get-field (place indicator)
             (when place
               (let ((keyword (first place))
                     (value (second place)))
                 (if (eql keyword indicator)
                     value
                     (get-field (cddr place) indicator))))))
    (remove nil (reduce (lambda (accum slot-spec)
                          (let ((options (rest (ensure-list slot-spec))))
                            (union (list (get-field options :accessor)
                                         (get-field options :writer)
                                         (get-field options :reader))
                                   accum)))
                        (slot-specs-of definer)
                        :initial-value nil))))

(defmethod expand-definer ((definer class-definer))
  (expand-class-like-definer definer 'defclass))


;;; CONDITION DEFINER ROUTINES

(defclass condition-definer (class-definer) ()
  (:metaclass definer-class))

(defmethod expand-definer ((definer condition-definer))
  (expand-class-like-definer definer 'define-condition))


;;; STRUCT DEFINER ROUTINES

(defclass struct-definer (definer)
  ((documentation :accessor documentation-of)
   (struct-options :accessor struct-options-of)
   (slot-descs :accessor slot-descs-of))
  (:metaclass definer-class))

(defmethod available-definer-options ((definer struct-definer))
  (list #\e #\a #\s))

(defmethod restricted-definer-options ((definer struct-definer))
  nil)

(defmethod initialize-definer ((definer struct-definer))
  (destructuring-bind (name &rest rest) (forms-of definer)
    (destructuring-bind (documentation &rest slot-descs)
        (if (stringp (car rest)) rest (cons nil rest))
      (setf (name-of definer) (first (ensure-list name))
            (struct-options-of definer) (rest (ensure-list name))
            (documentation-of definer) documentation
            (slot-descs-of definer) slot-descs)))
  (validate-definer-options definer)
  definer)

(defun extract-struct-accessors (definer)
  (mapcar (lambda (slot) (intern (format nil "~:@(~s-~s~)" (name-of definer) slot)))
          (extract-slots definer)))

(defmethod expand-definer ((definer struct-definer))
  `(progn
     (defstruct (,(name-of definer) ,@(struct-options-of definer))
       ,@(when (documentation-of definer) `(,(documentation-of definer)))
       ,@(slot-descs-of definer))
     ,@(when (has-option-p definer #\e) `((export ',(name-of definer))))
     ,@(when (has-option-p definer #\s) `((export ',(extract-slots definer))))
     ,@(when (has-option-p definer #\a)
         `((export ',(extract-struct-accessors definer))))))
