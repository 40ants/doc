;;;; STRUCTURE-ACCESSOR locative

(define-locative-type structure-accessor ()
  "This is a synonym of FUNCTION with the difference that the often
  ugly and certainly uninformative lambda list will not be printed.")

(defmethod locate-object ((symbol symbol)
                          (locative-type (eql 'structure-accessor))
                          locative-args)
  ;; Signal an error if it doesn't exist.
  (or (ignore-errors (symbol-function symbol))
      (locate-error))
  (make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document ((symbol symbol)
                                (locative-type (eql 'structure-accessor))
                                locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (print-end-bullet stream)
  (with-dislocated-symbols ((list symbol))
    (maybe-print-docstring symbol 'function stream)))

(defmethod locate-and-find-source (symbol
                                   (locative-type (eql 'structure-accessor))
                                   locative-args)
  (declare (ignore locative-args))
  ;; Some implementations can not find the source location of the
  ;; accessor function, so fall back on FIND-ONE-LOCATION.
  (let ((location (find-source (symbol-function symbol))))
    (if (eq :error (first location))
        (find-one-location (swank-backend:find-definitions symbol)
                           '("function" "operator"))
        location)))
