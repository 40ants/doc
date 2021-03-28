;;;; COMPILER-MACRO locative

(define-locative-type compiler-macro ())

(defmethod locate-object (symbol (locative-type (eql 'compiler-macro))
                          locative-args)
  (unless (compiler-macro-function symbol)
    (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'compiler-macro))
                                locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  (let ((arglist (swank-backend:arglist symbol)))
    (print-arglist arglist stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((macro-arg-names arglist))
      (maybe-print-docstring symbol 'function stream))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'compiler-macro))
                                   locative-args)
  (declare (ignore locative-args))
  #-allegro
  (find-source (compiler-macro-function symbol))
  #+allegro
  (find-one-location (swank-backend:find-definitions symbol)
                     '("compiler-macro")))
