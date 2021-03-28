;;;; PACKAGE locative

(define-locative-type package ())

(defmethod locate-object (symbol (locative-type (eql 'package))
                          locative-args)
  (assert (= 0 (length locative-args)))
  (or (find-package symbol) (locate-error)))

(defmethod canonical-reference ((package package))
  (make-reference (package-name package) 'package))

(defmethod document-object ((package package) stream)
  (let ((symbol (package-name package)))
    (print-bullet package stream)
    (print-end-bullet stream)
    (with-dislocated-symbols ((list symbol))
      (maybe-print-docstring package t stream))))
