;;;; CONSTANT locative

(define-locative-type constant (&optional initform)
  "Refers to a DEFCONSTANT. INITFORM, or if not specified,
  the value of the constant is included in the documentation.")

(defmethod locate-object (symbol (locative-type (eql 'constant)) locative-args)
  (assert (<= (length locative-args) 1))
  (assert (constantp symbol))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'constant))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (print-arglist (prin1-to-string (cond (initformp
                                           initform)
                                          ((boundp symbol)
                                           (symbol-value symbol))
                                          (t
                                           "<unbound>")))
                   stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((list symbol))
      (maybe-print-docstring symbol locative-type stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'constant))
                                   locative-args)
  (declare (ignore locative-args))
  (find-one-location (swank-backend:find-definitions symbol)
                     '("defconstant" "constant" "variable")))
