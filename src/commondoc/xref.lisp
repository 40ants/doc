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
  (:export
   #:make-xref
   #:xref
   #:xref-name
   #:xref-symbol
   #:xref-locative
   #:fill-locatives
   #:extract-symbols))
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
    
    (cl-ppcre:do-matches (start end "([A-Z][A-Z-]+::?)?[+*@]?[A-Z][A-Z-]+[+*]?" text)
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
             (1- (length text)))
      (push (common-doc:make-text (subseq text processed-to-idx))
            new-nodes))

    (if new-nodes
        (common-doc:make-content (nreverse new-nodes))
        node)))


(defun extract-symbols (node)
  "Extracts non marked up symbols from COMMON-DOC:TEXT-NODE and replaces them with XREF objects."
  (flet ((extractor (node)
           (typecase node
             (common-doc:text-node (extract-symbols-from-text node))
             (t node))))
    (40ants-doc/commondoc/mapper:map-nodes node #'extractor)))


(defgeneric link-text (object)
  (:documentation "Returns a string to be used as a text of `<a href=\"\">(link-text object)</a>` element.

                   By default, a symbol name will be used.")
  
  (:method ((object t))
    nil))


(defun replace-references (node known-references)
  "Replaces XREF with COMMON-DOC:WEB-LINK.

   Returns links which were not replaced because there wasn't
   a corresponding reference in the KNOWN-REFERENCES argument."
  
  (flet ((replacer (node)
           (typecase node
             (xref
              (let* ((text (xref-name node))
                     (symbol (xref-symbol node))
                     (locative (xref-locative node))
                     (found-references
                       (loop for reference in known-references
                             when (and (eql (40ants-doc/reference::reference-object reference)
                                            symbol)
                                       (or (null locative)
                                           (eql (40ants-doc/reference::reference-locative-type reference)
                                                locative)))
                             collect reference)))

                (cond
                  (found-references
                   (labels ((reference-to-uri (reference)
                              (40ants-doc/utils::html-safe-name
                               (40ants-doc/reference::reference-to-anchor reference)))
                            (make-link (reference text)
                              (common-doc:make-document-link nil
                                                             (reference-to-uri reference)
                                                             (common-doc:make-code
                                                              (common-doc:make-text text)))))

                     (cond ((= (length found-references) 1)
                            (let* ((reference (first found-references))
                                   (object (40ants-doc/reference::resolve reference))
                                   (text (or (link-text object)
                                             text)))
                              (make-link (first found-references)
                                         text)))
                           (t
                            (common-doc:make-content
                             (append (list (common-doc:make-code
                                            (common-doc:make-text text))
                                           (common-doc:make-text " ("))
                                     (loop for reference in found-references
                                           for index upfrom 1
                                           for text = (format nil "~A" index)
                                           collect (make-link reference text)
                                           unless (= index (length found-references))
                                           collect (common-doc:make-text " "))
                                     (list (common-doc:make-text ")"))))))))
                  
                  (t node))))
             (t
              node))))
    (40ants-doc/commondoc/mapper:map-nodes node #'replacer)))


(defun collect-references (node &aux results)
  "Returns a list of 40ANTS-DOC/REFERENCE:REFERENCE objects"
  
  (flet ((collector (node)
           (when (typep node '40ants-doc/commondoc/bullet::bullet)
             (push (40ants-doc/commondoc/bullet::bullet-reference node)
                   results))
           (when (typep node '40ants-doc/commondoc/section::documentation-section)
             (push (40ants-doc/reference-api::canonical-reference
                    (40ants-doc/commondoc/section:section-definition node))
                   results))
           node))
    (40ants-doc/commondoc/mapper:map-nodes node #'collector))

  results)



(define-emitter (obj xref)
                "Emit an reference which was not processed by REPLACE-REFERENCES."
                (with-html
                  (:code :class "unresolved-reference"
                         ;; Later we'll need to create a separate CSS with color theme
                         :style "color: magenta"
                         (xref-name obj))))
