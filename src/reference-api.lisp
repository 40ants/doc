(defpackage #:40ants-doc/reference-api
  (:use #:cl))
(in-package 40ants-doc/reference-api)


(defgeneric canonical-reference (object)
  (:documentation "Return a 40ANTS-DOC/REFERENCE::REFERENCE that resolves to OBJECT."))


(defgeneric collect-reachable-objects (object)
  (:documentation "Return a list of objects representing all things
  that would be documented in a (40ANTS-DOC/DOCUMENT::DOCUMENT OBJECT) call.
  For sections this is simply the union of references reachable from
  references in 40ANTS-DOC::SECTION-ENTRIES. The returned objects can be anything
  provided that CANONICAL-REFERENCE works on them. The list need not
  include OBJECT itself.

  One only has to specialize this for new container-like objects."))

(defgeneric format-reference (obj name ref link))
