(defpackage #:40ants-doc/args
  (:use #:cl)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/reference))
(in-package 40ants-doc/args)


;;; Return the names of the function arguments in ARGLIST that's a
;;; lambda list. Handles &KEY, &OPTIONAL, &REST.
(defun function-arg-names (arglist)
  (unless (eq arglist :not-available)
    (mapcar (lambda (arg)
              (if (and (listp arg)
                       (symbolp (first arg)))
                  (first arg)
                  arg))
            arglist)))

;;; Return the names of the arguments in ARGLIST that's a macro lambda
;;; list.
(defun macro-arg-names (arglist)
  (unless (eq arglist :not-available)
    (let ((names ()))
      (labels ((foo (arglist)
                 (let ((seen-special-p nil))
                   (loop for arg in arglist
                         do (cond ((member arg '(&key &optional &rest &body))
                                   (setq seen-special-p t))
                                  ((symbolp arg)
                                   (push arg names))
                                  (seen-special-p
                                   (when (symbolp (first arg))
                                     (push (first arg) names)))
                                  (t
                                   (foo arg)))))))
        (foo arglist))
      (reverse names))))

;;; Add a dummy page with for references to SYMBOLS whose locative is
;;; ARGUMENT. If an ARGUMENT reference is present for a symbol, it
;;; will surely be marked up as code, but it's not linkified in the
;;; absence of an explicit locative even if it the symbol refers to
;;; other things with different locatives.
(defmacro with-dislocated-symbols ((symbols) &body body)
  `(40ants-doc/page::with-pages ((list (40ants-doc/page::make-page
                       :references (mapcar (lambda (symbol)
                                             (40ants-doc/reference::make-reference symbol
                                                                                   '40ants-doc/locatives/dislocated::dislocated))
                                           ,symbols))))
     ,@body))
