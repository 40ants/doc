(uiop:define-package #:40ants-doc/small
  (:use #:cl)
  ;; TODO: Все эти locatives надо сделать доступными в минимальном пакете
  ;; может быть не загружать сами модули, но сделать так, чтобы на них можно было ссылаться
  ;; А может вовсе удалить этот модуль?
  (:import-from #:40ants-doc/locatives/section)
  (:import-from #:40ants-doc/locatives/function)
  (:import-from #:40ants-doc/locatives/dislocated)
  (:import-from #:40ants-doc/locatives/class)
  (:import-from #:40ants-doc/locatives/asdf-system)
  (:import-from #:40ants-doc/locatives/argument)
  (:import-from #:40ants-doc/locatives/compiler-macro)
  (:import-from #:40ants-doc/locatives/constant)
  (:import-from #:40ants-doc/locatives/variable)
  (:import-from #:40ants-doc/locatives/glossary)
  (:import-from #:40ants-doc/locatives/locative)
  (:import-from #:40ants-doc/locatives/macro)
  (:import-from #:40ants-doc/locatives/generic-function)
  (:import-from #:40ants-doc/locatives/method)
  (:import-from #:40ants-doc/locatives/package)
  (:import-from #:40ants-doc/locatives/restart)
  (:import-from #:40ants-doc/locatives/slots)
  (:import-from #:40ants-doc/locatives/structure-accessor)
  (:import-from #:40ants-doc/locatives/symbol-macro)
  (:import-from #:40ants-doc/locatives/type)
  (:import-from #:40ants-doc/locatives/include))
(in-package #:40ants-doc/small)
