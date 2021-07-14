(uiop:define-package #:40ants-doc/commondoc/xref
  (:use #:cl)
  (:import-from #:40ants-doc/commondoc/bullet)
  (:import-from #:common-doc)
  (:import-from #:40ants-doc/commondoc/html
                #:with-html)
  (:import-from #:common-html.emitter
                #:define-emitter)
  (:import-from #:40ants-doc/commondoc/utils
                #:read-locative
                #:left-word
                #:right-word)
  (:import-from #:40ants-doc/swank)
  (:import-from #:cl-ppcre)
  (:import-from #:40ants-doc/ignored-words
                #:ignored-words
                #:supports-ignored-words-p)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/commondoc/mapper)
  (:export
   #:make-xref
   #:xref
   #:xref-name
   #:xref-symbol
   #:xref-locative
   #:fill-locatives
   #:extract-symbols
   #:link-text))
(in-package 40ants-doc/commondoc/xref)


(defclass xref (common-doc:document-node)
  ((name :accessor xref-name
         :initarg :name
         :type string
         :documentation "Original text, found in a documentation string")
   (symbol :accessor xref-symbol
           :initarg :symbol
           :type (or null symbol)
           :documentation "A symbol, matched to a XREF-NAME.

                           I can be NIL if no symbol was found.
                           In this case a warning will be shown.")
   (locative :accessor xref-locative
             :initarg :locative
             :type (or null symbol)
             :documentation "Sometime xref might be followed by a locative name.
                             In this case this slot will be filled with a corresponding
                             locative symbol from 40ANTS-DOC/LOCATIVES package."))
  (:documentation "A link some entity, refered in markdown as a link like [Some text][the-id]
                   or just being UPPERCASED-SYMBOL mentioned."))


(defun make-xref (name &key symbol locative)
  (check-type name string)
  (check-type symbol (or null symbol))
  (check-type locative (or null symbol))
  
  (make-instance 'xref
                 :name name
                 :symbol symbol
                 :locative locative))


(defun fill-locatives (node)
  "This goes through nodes tree and fills LOCATIVE slot of XREF objects
   in case if this XREF is prepended or followed by a locative word like
   \"macro\" or \"function\"."

  (let ((locative-on-the-left nil)
        (prev-xref nil))
    (labels ((filler (node)
               (typecase node
                 (common-doc:text-node
                  (cond
                    ((null prev-xref)
                     (setf locative-on-the-left
                           (read-locative (right-word node))))
                    ((null (xref-locative prev-xref))
                     (setf (xref-locative prev-xref)
                           (read-locative (left-word node)))
                     (setf prev-xref
                           nil))))
                 (xref
                  (cond
                    (locative-on-the-left
                     (setf (xref-locative node)
                           locative-on-the-left)
                     (setf locative-on-the-left
                           nil))
                    (t
                     (setf prev-xref
                           node)))))
               (values node)))
      (40ants-doc/commondoc/mapper:map-nodes node
                                             #'filler)))
  node)


(defun extract-symbols-from-text (node)
  ;; TODO: Find if this a replacement for FIND-DEFINITIONS-FIND-SYMBOL-OR-PACKAGE.
  (let ((text (common-doc:text node))
        (new-nodes nil)
        (processed-to-idx 0))

    (cl-ppcre:do-matches (start end "([A-Z0-9][A-Z0-9-]+::?)?[+*@]?[A-Z][A-Z0-9-]*[A-Z0-9]+[+*]?" text)
      (when (> start processed-to-idx)
        (push (common-doc:make-text (subseq text processed-to-idx start))
              new-nodes))

      (let* ((symbol-name (subseq text start end))
             (symbol (40ants-doc/swank::read-locative-from-string symbol-name)))
        (push (make-xref symbol-name
                         :symbol symbol)
              new-nodes))

      (setf processed-to-idx end))

    (when (< processed-to-idx
             (length text))
      (push (common-doc:make-text (subseq text processed-to-idx))
            new-nodes))

    (if new-nodes
        (common-doc:make-content (nreverse new-nodes))
        node)))


;; (defmethod 40ants-doc/utils::object-package ((obj common-doc:text-node))
;;   *package*)

(defmethod 40ants-doc/utils::object-package ((obj common-doc:document))
  *package*)

(defmethod 40ants-doc/utils::object-package ((obj common-doc:document-node))
  *package*)

;; (defmethod 40ants-doc/utils::object-package ((obj 40ants-doc/commondoc/bullet::bullet))
;;   *package*)


(defun extract-symbols (node)
  "Extracts non marked up symbols from COMMON-DOC:TEXT-NODE and replaces them with XREF objects."
  (let ((*package* *package*)
        (packages-stack nil))
    ;; Here we we need to change *package*
    ;; to make sure, that all symbol mentions are parsed as if we being
    ;; in the package where DOCUMENTATION-SECTION was defined.
    (flet ((set-package (node)
             (let ((package (40ants-doc/utils::object-package node)))
               (push *package* packages-stack)
               (setf *package* package)))
           (reset-package (node)
             (declare (ignore node))
             (setf *package*
                   (pop packages-stack)))
           (extractor (node)
             (typecase node
               (common-doc:text-node (extract-symbols-from-text node))
               (t node))))
      (40ants-doc/commondoc/mapper:map-nodes node #'extractor
                                             :on-going-down #'set-package
                                             :on-going-up #'reset-package))))


(defgeneric link-text (object)
  (:documentation "Returns a string to be used as a text of `<a href=\"\">(link-text object)</a>` element.

                   By default, a symbol name will be used.")
  
  (:method ((object t))
    nil))


(define-emitter (obj xref)
  "Emit an reference which was not processed by 40ANTS-DOC/COMMONDOC/PAGE::REPLACE-REFERENCES."
  (with-html
    (:code :class "unresolved-reference"
           :title "Reference not found."
           (xref-name obj))))
