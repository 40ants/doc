;;;; VARIABLE locative

(define-locative-type variable (&optional initform)
  "Refers to a global special variable. INITFORM, or if not specified,
  the global value of the variable is included in the documentation.")

(defmethod locate-object (symbol (locative-type (eql 'variable)) locative-args)
  (assert (<= (length locative-args) 1))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'variable))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (multiple-value-bind (value unboundp) (symbol-global-value symbol)
      (print-arglist (prin1-to-string (cond (initformp initform)
                                            (unboundp "-unbound-")
                                            (t value)))
                     stream))
    (print-end-bullet stream)
    (with-dislocated-symbols ((list symbol))
      (maybe-print-docstring symbol locative-type stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'variable))
                                   locative-args)
  (declare (ignore locative-args))
  (find-one-location (swank-backend:find-definitions symbol)
                     '("variable" "defvar" "defparameter"
                       "special-declaration")))

(defvar end-of-variable-example)
