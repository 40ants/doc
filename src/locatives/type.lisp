;;;; TYPE locative

(define-locative-type type ()
  "TYPE can refer to classes as well, but it's better style to use the
  more specific CLASS locative type for that. Another difference to
  CLASS is that an attempt is made at printing the arguments of type
  specifiers.")

(defmethod locate-object (symbol (locative-type (eql 'type)) locative-args)
  (unless (swank-backend:type-specifier-p 'symbol)
    (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'type)) locative-args
                                stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (let ((arglist (swank-backend:type-specifier-arglist symbol)))
    (when (and arglist (not (eq arglist :not-available)))
      (write-char #\Space stream)
      (print-arglist arglist stream)))
  (print-end-bullet stream)
  (with-dislocated-symbols ((list symbol))
    (maybe-print-docstring symbol 'type stream)))

(defmethod locate-and-find-source (symbol (locative-type (eql 'type))
                                   locative-args)
  (declare (ignore locative-args))
  (find-one-location (swank-backend:find-definitions symbol)
                     '("type" "class")))
