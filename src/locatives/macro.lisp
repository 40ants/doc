;;;; MACRO locative

(define-locative-type macro ())

(defmethod locate-object (symbol (locative-type (eql 'macro)) locative-args)
  (unless (macro-function symbol)
    (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'macro))
                                locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  (let ((arglist (swank-backend:arglist symbol)))
    (print-arglist arglist stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((macro-arg-names arglist))
      (maybe-print-docstring symbol 'function stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'macro))
                                   locative-args)
  (declare (ignore locative-args))
  (find-source (macro-function symbol)))
