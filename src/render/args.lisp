(defpackage #:40ants-doc/render/args
  (:use #:cl)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/markdown/transform))
(in-package 40ants-doc/render/args)


(defun print-arglist (arglist stream)
  (let ((string (cond ((stringp arglist)
                       ;; must be escaped markdown
                       arglist)
                      ((eq arglist :not-available)
                       "")
                      (t (arglist-to-string arglist)))))
    (if 40ants-doc/builder/vars::*document-mark-up-signatures*
        (if (eq 40ants-doc/builder/printer::*format* :html)
            (format stream "<span class=\"locative-args\">~A</span>" string)
            (40ants-doc/utils::italic string stream))
        (format stream "~A" string))))

;;; Print arg names without the package prefix to a string. The
;;; default value with prefix. Works for macro arglists too.
(defun arglist-to-string (arglist)
  (with-output-to-string (out)
    (let ((seen-special-p nil)
          (*print-pretty* t)
          (*print-right-margin* nil))
      (labels ((resolve* (object)
                 (if (and 40ants-doc/builder/vars::*document-mark-up-signatures*
                          ;; KLUDGE: github has trouble displaying
                          ;; things like '`*package*`, so disable
                          ;; this.
                          (eq 40ants-doc/builder/printer::*format* :html))
                     (40ants-doc/markdown/transform::replace-known-references
                      (40ants-doc/utils::prin1-and-escape-markdown object))
                     (40ants-doc/utils::prin1-and-escape-markdown object)))
               (foo (arglist level)
                 (unless (= level 0)
                   (format out "("))
                 (loop for i upfrom 0
                       for arg in arglist
                       do (unless (zerop i)
                            (format out " "))
                          (cond ((member arg '(&key &optional &rest &body))
                                 (setq seen-special-p t)
                                 (format out "~A"
                                         (40ants-doc/utils::prin1-and-escape-markdown arg)))
                                ((symbolp arg)
                                 (format out "~A"
                                         (40ants-doc/utils::escape-markdown
                                          (symbol-name arg))))
                                ((atom arg)
                                 (format out "~A"
                                         (40ants-doc/utils::prin1-and-escape-markdown arg)))
                                (seen-special-p
                                 (if (symbolp (first arg))
                                     (format out "(~A~{ ~A~})"
                                             (40ants-doc/utils::escape-markdown
                                              (symbol-name (first arg)))
                                             (mapcar #'resolve* (rest arg)))
                                     (format out "~A"
                                             (40ants-doc/utils::prin1-and-escape-markdown arg))))
                                (t
                                 (foo arg (1+ level)))))
                 (unless (= level 0)
                   (format out ")"))))
        (foo arglist 0)))))
