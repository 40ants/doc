(defpackage #:40ants-doc/highlight
  (:use #:cl)
  (:import-from #:dexador)
  (:import-from #:log4cl)
  (:import-from #:alexandria
                #:when-let
                #:write-string-into-file
                #:write-byte-vector-into-file
                #:read-file-into-string)
  (:import-from #:cl-cookie
                #:cookie-value
                #:cookie-name
                #:make-cookie-jar
                #:cookie-jar-cookies)
  (:import-from #:tmpdir
                #:with-tmpdir)
  (:import-from #:trivial-extract
                #:extract-zip))
(in-package 40ants-doc/highlight)


(defvar *supported-languages*
  '("1c"
    "abnf"
    "accesslog"
    "actionscript"
    "ada"
    "angelscript"
    "apache"
    "applescript"
    "arcade"
    "arduino"
    "armasm"
    "asciidoc"
    "aspectj"
    "autohotkey"
    "autoit"
    "avrasm"
    "awk"
    "axapta"
    "bash"
    "basic"
    "bnf"
    "brainfuck"
    "c"
    "cal"
    "capnproto"
    "ceylon"
    "clean"
    "clojure-repl"
    "clojure"
    "cmake"
    "coffeescript"
    "coq"
    "cos"
    "cpp"
    "crmsh"
    "crystal"
    "csharp"
    "csp"
    "css"
    "d"
    "dart"
    "delphi"
    "diff"
    "django"
    "dns"
    "dockerfile"
    "dos"
    "dsconfig"
    "dts"
    "dust"
    "ebnf"
    "elixir"
    "elm"
    "erb"
    "erlang-repl"
    "erlang"
    "excel"
    "fix"
    "flix"
    "fortran"
    "fsharp"
    "gams"
    "gauss"
    "gcode"
    "gherkin"
    "glsl"
    "gml"
    "go"
    "golo"
    "gradle"
    "groovy"
    "haml"
    "handlebars"
    "haskell"
    "haxe"
    "hsp"
    "http"
    "hy"
    "inform7"
    "ini"
    "irpf90"
    "isbl"
    "java"
    "javascript"
    "jboss-cli"
    "json"
    "julia-repl"
    "julia"
    "kotlin"
    "lasso"
    "latex"
    "ldif"
    "leaf"
    "less"
    "lisp"
    "livecodeserver"
    "livescript"
    "llvm"
    "lsl"
    "lua"
    "makefile"
    "markdown"
    "mathematica"
    "matlab"
    "maxima"
    "mel"
    "mercury"
    "mipsasm"
    "mizar"
    "mojolicious"
    "monkey"
    "moonscript"
    "n1ql"
    "nestedtext"
    "nginx"
    "nim"
    "nix"
    "node-repl"
    "nsis"
    "objectivec"
    "ocaml"
    "openscad"
    "oxygene"
    "parser3"
    "perl"
    "pf"
    "pgsql"
    "php-template"
    "php"
    "plaintext"
    "pony"
    "powershell"
    "processing"
    "profile"
    "prolog"
    "properties"
    "protobuf"
    "puppet"
    "purebasic"
    "python-repl"
    "python"
    "q"
    "qml"
    "r"
    "reasonml"
    "rib"
    "roboconf"
    "routeros"
    "rsl"
    "ruby"
    "ruleslanguage"
    "rust"
    "sas"
    "scala"
    "scheme"
    "scilab"
    "scss"
    "shell"
    "smali"
    "smalltalk"
    "sml"
    "sqf"
    "sql"
    "stan"
    "stata"
    "step21"
    "stylus"
    "subunit"
    "swift"
    "taggerscript"
    "tap"
    "tcl"
    "thrift"
    "tp"
    "twig"
    "typescript"
    "vala"
    "vbnet"
    "vbscript-html"
    "vbscript"
    "verilog"
    "vhdl"
    "vim"
    "wasm"
    "wren"
    "x86asm"
    "xl"
    "xml"
    "xquery"
    "yaml"
    "zephir"))


(defun to-downcased-string (thing)
  (string-downcase
   (etypecase thing
     (symbol (symbol-name thing))
     (string thing))))

(defun normalize (lang)
  (let ((result (to-downcased-string lang)))
    (unless (member result *supported-languages* :test #'string=)
      (error "Language \"~A\" is not supported by highlight.js"
             result))
    result))

(defun normalize-langs (languages)
  (let* ((languages (if (eql languages :all)
                        *supported-languages*
                        (uiop:ensure-list languages)))
         (normalized (mapcar #'normalize languages))
         (sorted (sort normalized
                       #'string<)))
    sorted))

(defun generate-meta-data (languages theme)
  (format nil "languages: ~{~a~^, ~}~%theme: ~A~%"
          languages
          (to-downcased-string theme)))

(defun download-highlight-js (languages &key (to "./")
                                             (theme "default"))
  (with-tmpdir (tmpdir)
    (let* ((languages (normalize-langs languages))
           (to (uiop:ensure-directory-pathname to))
           (metadata-path (uiop:merge-pathnames* "METADATA"
                                                 to))
           (metadata (generate-meta-data languages theme)))

      (cond
        ((and (probe-file metadata-path)
              (string= (read-file-into-string metadata-path)
                       metadata))
         (log:info "METADATA file lists same languages and theme. Skipping download of Highlight.js"))
        (t
         (log:info "Downloading Highlight.js")
         (let* ((url "https://highlightjs.org/download/")
                (jar (make-cookie-jar))
                (cookies (progn (dex:get url :cookie-jar jar)
                                (cookie-jar-cookies jar)))
                (csrftoken (when-let ((cookie (find "csrftoken" cookies
                                                    :key #'cookie-name
                                                    :test #'string-equal)))
                             (cookie-value cookie)))
                (post-data (append (list (cons "csrfmiddlewaretoken" csrftoken))
                                   (loop for lang in languages
                                         for normalized-lang = (normalize lang)
                                         collect (cons (format nil "~A.js" lang)
                                                       "on"))))
                (headers (list (cons "Referer" url)))
                (response (dex:post url
                                    :content post-data
                                    :headers headers
                                    :cookie-jar jar))
                (path (uiop:merge-pathnames* #P"archive.zip" tmpdir)))

           (ensure-directories-exist path)
           (ensure-directories-exist to)
           
           (write-byte-vector-into-file response path
                                        :if-exists :supersede)
           (extract-zip path)

           (uiop:copy-file (uiop:merge-pathnames* "highlight.min.js" tmpdir)
                           (uiop:merge-pathnames* "highlight.min.js" to))

           (let* ((theme (to-downcased-string theme))
                  (theme-path (uiop:merge-pathnames* (format nil "styles/~A.min.css" theme)
                                                     tmpdir)))
             (unless (probe-file theme-path)
               (error "Theme \"~A\" was is not supported by Highlight.js"
                      theme))
             (uiop:copy-file theme-path
                             (uiop:merge-pathnames* "highlight.min.css" to)))

           (write-string-into-file metadata metadata-path
                                   :if-exists :supersede))))))
  (values))
