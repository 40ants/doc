;;;; CLASS and CONDITION locatives

(define-locative-type class ())

(define-locative-type condition ())

(defmethod locate-object (symbol (locative-type (eql 'class)) locative-args)
  (declare (ignore locative-args))
  (or (find-class symbol :errorp nil)
      (locate-error)))

(defmethod locate-object (symbol (locative-type (eql 'condition))
                          locative-args)
  (assert (= 0 (length locative-args)))
  (let ((class (find-class symbol :errorp nil)))
    (unless (subtypep class 'condition)
      (locate-error))
    class))

(defmethod canonical-reference ((class class))
  (if (subtypep class 'condition)
      (make-reference (class-name class) 'condition)
      (make-reference (class-name class) 'class)))

(defmethod document-object ((class class) stream)
  (let* ((conditionp (subtypep class 'condition))
         (symbol (class-name class))
         (superclasses
           (remove-if (lambda (name)
                        (or (eq name 'standard-object)
                            (and conditionp (eq name 'condition))))
                      (mapcar #'class-name
                              (swank-mop:class-direct-superclasses class)))))
    (print-bullet class stream)
    (when superclasses
      (write-char #\Space stream)
      (if *document-mark-up-signatures*
          (print-arglist (mark-up-superclasses superclasses) stream)
          (print-arglist superclasses stream)))
    (print-end-bullet stream)
    (with-dislocated-symbols ((list symbol))
      (maybe-print-docstring class t stream))))

(defun mark-up-superclasses (superclasses)
  (with-output-to-string (stream)
    (loop for class in superclasses
          for i upfrom 0
          do (let ((reference (make-reference class 'class)))
               (let ((name (escape-markdown (prin1-to-string class))))
                 (unless (zerop i)
                   (format stream " "))
                 (if (find-known-reference reference)
                     (format stream "[~A][~A]" name
                             (link-to-reference reference))
                     (format stream "~A" name)))))))

(defun find-known-reference (reference)
  (find reference *references* :test #'reference=))
