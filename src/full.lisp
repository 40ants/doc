(defpackage #:40ants-doc/full
  (:use #:cl)
  (:import-from #:40ants-doc/builder)
  (:import-from #:40ants-doc/builder/reference)

  ;; TODO: Все эти locatives надо сделать доступными в минимальном пакете
  ;; может быть не загружать сами модули, но сделать так, чтобы на них можно было ссылаться
  (:import-from #:40ants-doc/locatives/section)
  (:import-from #:40ants-doc/locatives/function)
  (:import-from #:40ants-doc/locatives/dislocated)
  (:import-from #:40ants-doc/locatives/class)
  (:import-from #:40ants-doc/locatives/asdf-system)
  (:import-from #:40ants-doc/locatives/argument)
  (:import-from #:40ants-doc/locatives/compiler-macro)
  (:import-from #:40ants-doc/locatives/constant)
  (:import-from #:40ants-doc/locatives/variable)
  
  (:import-from #:40ants-doc/render/navigation)
  (:import-from #:40ants-doc/document/string)
  (:import-from #:40ants-doc/github))
(in-package 40ants-doc/full)