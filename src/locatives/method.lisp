;;;; METHOD locative

(define-locative-type method (method-qualifiers method-specializers)
  "See CL:FIND-METHOD for the description of the arguments.
  To refer to the default method of the three argument generic
  function FOO:

      (foo (method () (t t t)))")

(defmethod locate-object (symbol (locative-type (eql 'method))
                          locative-args)
  (assert (= 2 (length locative-args)))
  (destructuring-bind (qualifiers specializers) locative-args
    (or (ignore-errors
         (find-method (symbol-function symbol) qualifiers
                      (loop for specializer in specializers
                            collect (typecase specializer
                                      ;; SPECIALIZER can be a cons
                                      ;; like (:EQL :SOME-VALUE) ...
                                      (cons specializer)
                                      ;; or a type specifier denoting
                                      ;; a class:
                                      (t (find-class specializer))))))
        (locate-error))))

(defmethod canonical-reference ((method method))
  (make-reference (swank-mop:generic-function-name
                   (swank-mop:method-generic-function method))
                  `(method ,(swank-mop:method-qualifiers method)
                           ,(method-specializers-list method))))

;;; Return the specializers in a format suitable as the second
;;; argument to FIND-METHOD.
(defun method-specializers-list (method)
  (mapcar (lambda (spec)
            (typecase spec
              (swank-mop:eql-specializer
               `(eql ,(swank-mop:eql-specializer-object spec)))
              (t (swank-mop:class-name spec))))
          (swank-mop:method-specializers method)))

(defmethod document-object ((method method) stream)
  (let ((arglist (rest (method-for-inspect-value method))))
    (print-bullet method stream)
    (write-char #\Space stream)
    (print-arglist arglist stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((function-arg-names arglist))
      (maybe-print-docstring method t stream))))

;;;; These were lifted from the fancy inspector contrib and then
;;;; tweaked.

(defun method-specializers-for-inspect (method)
  """Return a "pretty" list of the method's specializers. Normal
  specializers are replaced by the name of the class, eql specializers
  are replaced by `(eql ,object)."""
  (mapcar (lambda (name spec)
            (let ((name (if (listp name) (first name) name)))
              (if (eq spec t)
                  name
                  (list name spec))))
          (swank-mop:method-lambda-list method)
          (method-specializers-list method)))

(defun method-for-inspect-value (method)
  """Returns a "pretty" list describing METHOD. The first element of
  the list is the name of generic-function method is specialized on,
  the second element is the method qualifiers, the rest of the list is
  the method's specialiazers (as per
  METHOD-SPECIALIZERS-FOR-INSPECT)."""
  (append (list (swank-mop:generic-function-name
                 (swank-mop:method-generic-function method)))
          (swank-mop:method-qualifiers method)
          (method-specializers-for-inspect method)))
