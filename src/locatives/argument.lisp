;;;; ARGUMENT locative

(define-locative-type argument ()
  """An alias for DISLOCATED, so the one can refer to an argument of a
  macro without accidentally linking to a class that has the same name
  as that argument. In the following example, FORMAT may link to
  CL:FORMAT (if we generated documentation for it):

  ```
  "See the FORMAT in DOCUMENT."
  ```

  Since ARGUMENT is a locative, we can prevent that linking by writing:

  ```
  "See the FORMAT argument of DOCUMENT."
  ```""")

(defmethod locate-object (symbol (locative-type (eql 'argument)) locative-args)
  (declare (ignore symbol locative-args))
  (locate-error))
