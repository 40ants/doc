;;;; LOCATIVE locative

(define-locative-type locative (lambda-list)
  "This is the locative for locatives. When `M-.` is pressed on
  `VARIABLE` in `(VARIABLE LOCATIVE)`, this is what makes it possible
  to land at the `(DEFINE-LOCATIVE-TYPE VARIABLE ...)` form.
  Similarly, `(LOCATIVE LOCATIVE)` leads to this very definition.")

(defmethod locate-object (symbol (locative-type (eql 'locative)) locative-args)
  (assert (endp locative-args))
  (or (ignore-errors (locative-lambda-list-method-for-symbol symbol))
      (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defun locative-lambda-list-method-for-symbol (symbol)
  (find-method #'locative-lambda-list () `((eql ,symbol))))

(defmethod locate-and-document (symbol (locative-type (eql 'locative))
                                locative-args stream)
  (let ((method (locative-lambda-list-method-for-symbol symbol))
        (lambda-list (locative-lambda-list symbol)))
    (locate-and-print-bullet locative-type locative-args symbol stream)
    (with-dislocated-symbols ((macro-arg-names lambda-list))
      (when lambda-list
        (write-char #\Space stream)
        (print-arglist lambda-list stream))
      (print-end-bullet stream)
      (with-dislocated-symbols ((list symbol))
        (maybe-print-docstring method t stream))))
  (format stream "~&"))

(defmethod locate-and-find-source (symbol (locative-type (eql 'locative))
                                   locative-args)
  (declare (ignore locative-args))
  (find-source (locative-lambda-list-method-for-symbol symbol)))
