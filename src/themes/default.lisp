(uiop:define-package #:40ants-doc/themes/default
  (:use #:cl)
  (:import-from #:40ants-doc/themes/api
                #:render-css
                #:render-page
                #:render-html-head
                #:render-page-header
                #:render-page-footer
                #:render-content
                #:render-sidebar
                #:render-sidebar-header
                #:render-sidebar-footer
                #:render-sidebar-content
                #:render-toc
                #:render-search-form)
  (:import-from #:lass)
  (:import-from #:40ants-doc/commondoc/html
                #:with-html)
  (:import-from #:40ants-doc/utils
                #:make-relative-path)
  (:import-from #:40ants-doc/rewrite)
  (:import-from #:40ants-doc
                #:defsection)
  (:export
   #:default-theme))
(in-package 40ants-doc/themes/default)


(defsection @defining-a-theme (:title "Defining a Custom Theme"
                               :ignore-words ("HEAD"
                                              "40A"))
  "Out of the box, 40ANTS-DOC system supports three color themes:

   - 40ANTS-DOC/THEMES/DEFAULT:DEFAULT-THEME
   - 40ANTS-DOC/THEMES/LIGHT:LIGHT-THEME
   - 40ANTS-DOC/THEMES/DARK:DARK-THEME

   You can pass these names as THEME argument to the 40ANTS-DOC/BUILDER:RENDER-TO-FILES
   function. Or you can define your own theme.

   Theme allows to control HTML page rendering, colors and code highlighting.

   ## Changing Colors

   The simplest way to customize theme is to redefine some colors using CSS.
   Here is how to set orange page background:

   ```lisp
   (defclass my-theme (default-theme)
     ())

   (defmethod 40ants-doc/themes/api:render-css ((theme my-theme))
     (concatenate
      'string
      (call-next-method)
     
      (lass:compile-and-write
       `(body
         :background orange))))
   ```

   Also you might want to redefine a color theme for code highlighter:

   ```
   (defmethod 40ants-doc/themes/api:highlight-theme ((theme my-theme))
     \"atom-one-light\")
   ```

   Talking about code highlighting, you can also redefine a list of
   languages to highlight:

   ```lisp
   (defmethod 40ants-doc/themes/api:highlight-languages ((theme my-theme))
     (list \"lisp\"
           \"python\"
           \"bash\"))
   ```

   ## Changing Page Layout

   The main entry-point for page rendering is
   RENDER-PAGE generic-function. It calls all other
   rendering functions.

   If you are inheriting your theme class from 40ANTS-DOC/THEMES/DEFAULT:DEFAULT-THEME,
   then rendering functions will be called in the following order:

   ![Page Rendering Flow](static/rendering.png{width=600})

   On this page stripes on the right demonstrate order in which different rendering functions will be called:

   - green is [RENDER-PAGE][generic-function];
   - blue is [RENDER-PAGE-HEADER][generic-function];
   - violet is [RENDER-SIDEBAR][generic-function]
   - red is [RENDER-SIDEBAR-HEADER][generic-function]
   - yellow is [RENDER-SIDEBAR-CONTENT][generic-function];
   - orange is [RENDER-SIDEBAR-FOOTER][generic-function];
   - salad green is [RENDER-CONTENT][generic-function];
   - pink is [RENDER-PAGE-FOOTER][generic-function].

   Some of these methods might call [RENDER-TOC][generic-function] and
   [RENDER-SEARCH-FORM][generic-function] to display a table of content
   and a table form. Also, you might want to redefine RENDER-HTML-HEAD generic-function
   to change html page metadata such as included stylesheets and js files, page title, etc.

   If you want to introduce changes, it is better to inherit from existing theme class
   and to define a few methods to change only needed properties. For example, here is
   a [theme I've made][my-theme] for all 40Ants projects. I've added header, footer and made colors match
   the main site.

   [my-theme]: https://github.com/40ants/40ants-doc-theme-40ants/blob/master/theme.lisp

   ## Available Themes"
  
  (40ants-doc/themes/default:default-theme class)
  (40ants-doc/themes/light:light-theme class)
  (40ants-doc/themes/dark:dark-theme class)
  
  "## Theme Definition Protocol"
  
  (40ants-doc/themes/api:highlight-languages generic-function)
  (40ants-doc/themes/api:highlight-theme generic-function)
  
  (40ants-doc/themes/api:render-css generic-function)
  
  (render-page generic-function)
  (render-html-head generic-function)
  (render-page-header generic-function)
  (render-page-footer generic-function)
  (render-content generic-function)
  (render-sidebar generic-function)
  (render-sidebar-header generic-function)
  (render-sidebar-footer generic-function)
  (render-sidebar-content generic-function)
  (render-toc generic-function)
  (render-search-form generic-function))


(defclass default-theme ()
  ())


(defmethod render-css ((theme default-theme))
  (lass:compile-and-write
   '(body
     :font-family "sans-serif"
     :margin "auto"
     :background-color "#FFFEFB"
     :color "#000000"

     ((:or h1 h2 h3 h4 h5 h6)
      :font-family "serif"
      :font-weight "bold"
      :text-shadow "0.05em 0.05em 0.02em #DDDDDD"
      (a.header-link
       :color "#DDD"
       :padding 0 4px
       :text-decoration none
       :border-bottom none
       :text-shadow none
       :visibility hidden))

     ((:and
       (:or h1 h2 h3 h4 h5 h6)
       :hover)
      (a.header-link
       :visibility visible)
      ((:and
        a.header-link
        :hover)
       :color "#777"))

     (h1
      :font-size 250%)
     (h2
      :font-size 200%
      :border-bottom "0.05em solid #CCCCCC")
     (h3
      :font-size 150%)
     (h4
      :font-size 130%)
     (h5
      :font-size 110%
      :margin-top 0.75em
      :margin-bottom 0.5em)
     (h6
      :font-size 100%
      :background-color inherit
      :color "#777777"
      (a
       :color "#777777"))

     (hr
      :height 0.2em
      :border 0
      :color "#CCCCCC"
      :background-color "#CCCCCC")

     ((:or p blockquote table pre)
      :line-height 1.5
      :margin "0.5em 0 0 0")

     ((:or ul ol dl)
      :padding-left 1.1em
      :list-style-position outside)

     (li
      :line-height 1.5
      :margin "0 0 0 0")

     (pre
      :background-color "#F5F3ED"
      :color "#000000"
      :border "0.1em solid #CCCCCC"
      :line-height 1.25em
      :overflow auto
      :margin "0.5em 0 0 0"
      :padding 0.2em)

     (code
      :color "#222222")
     
     (a
      :color "#222222"
      :text-decoration none
      :border-bottom 1px solid)

     (.locative-type
      (a
       :text-decoration none
       :border-bottom 0
       :font-weight bold))
     
     (.reference-object
      :background-color "#EBE8E2"
      :padding-left 0.3em
      :padding-right 0.3em
      :margin-left 0.2em
      :border "solid gray 1px"
      :font-weight bold
      (a
       :border-bottom none))

     (.locative-args
      :font-style italic
      :margin-left 0.2em
      :color "#777"
      (code
       :font-family sans-serif))

     (navigation
      :display block
      :visibility hidden
      :margin-bottom -1.5em
      (a
       :color "#CCCCCC"
       :text-shadow none
       :border-bottomi none))

     ((:and .outer-navigation :hover)
      (.navigation
       :visibility visible))

     (.highlighted
      :background "#adff2f")

     ;; Content

     (.page
      :margin 0
      :padding 0)
     
     ((.page > .content)
      :margin-left 40ex
      :padding-left 2.5em
      :max-width 85ex)
     
     (.sidebar
      :top 0px
      :left 0px
      :height 100%
      :width 40ex
      :max-width 33%
      :position fixed
      :overflow-y auto
      :overflow-x hidden
      :background "#333"
      :box-shadow inset -5px 0 5px 0px "#000"
      :color "#aaa"
      (p
       :padding 5px
       :margin 0
       :margin-left 10px
       :font-size 14px)
      (ul 
       :margin-left 10px
       :padding 0
       :list-style none)
      (li
       :line-height 1.0
       :padding 0
       :margin 0)
      (a
       :border-bottom none)
      (hr
       :height 0.05em
       :border 0
       :background "#777")
      (.toc-active 
       :background "#336699"
       :box-shadow inset -5px 0px 10px -5px "#000"))

     ((.sidebar > .header)
      (a
       :color "#777777"))

     ((.sidebar > .footer)
      :margin-left 1.5em
      :margin-top 2em
      :margin-bottom 1em
      (a
       :font-size 80%
       :color "#777777"))

     (form.search
      :margin-left 1.5em
      :margin-top 1.5em)
     
     (.page-toc
      (a
       :color "#fff"))

     (.menu-block 
      :padding-left 10px
      :margin-bottom 1em
      (a 
       :color "#fff"
       :border-bottom none))
     (.menu-block-title 
      :font-size 90%)

     (|#search-results|
      (.search
       (li
        :margin-bottom 1em)))

     (.unresolved-reference
      :color magenta))))

(defmethod 40ants-doc/themes/api:highlight-languages ((theme default-theme))
  '("lisp" "bash" "css" "json" "yaml" "plaintext" "xml" "markdown"))

(defmethod 40ants-doc/themes/api:highlight-theme ((theme default-theme))
  "atom-one-dark")

(defmethod 40ants-doc/themes/api:render-page ((theme default-theme) uri title
                                              &key toc content)
  (with-html
    (:html
     (:head
      (40ants-doc/themes/api:render-html-head theme uri title))
     (:body
      (:div :class "page"
            (40ants-doc/themes/api:render-page-header theme uri title)
            (40ants-doc/themes/api:render-sidebar theme uri toc)
            (40ants-doc/themes/api:render-content theme uri toc content)
            (40ants-doc/themes/api:render-page-footer theme uri))))))


(defmethod 40ants-doc/themes/api:render-page-header ((theme default-theme) uri title)
  (declare (ignore uri title)))

(defmethod 40ants-doc/themes/api:render-page-footer ((theme default-theme) uri)
  (declare (ignore uri)))


(defmethod 40ants-doc/themes/api:render-html-head ((theme default-theme) uri title)
  (let ((theme-uri (make-relative-path uri "theme.css"))
        (highlight-css-uri (make-relative-path uri "highlight.min.css"))
        (highlight-js-uri (make-relative-path uri "highlight.min.js"))
        (jquery-uri (make-relative-path uri "jquery.js"))
        (toc-js-uri (make-relative-path uri "toc.js")))
    (with-html
      (:meta :name "viewport"
             :content "width=device-width, initial-scale=1")
      (:title title)
      (:link :rel "stylesheet"
             :type "text/css"
             :href theme-uri)
      (:script :type "text/javascript"
               :src jquery-uri)
      (:script :type "text/javascript"
               :src toc-js-uri)
      (:link :rel "stylesheet"
             :type "text/css"
             :href highlight-css-uri)
      (:script :type "text/javascript"
               :src highlight-js-uri)
      (:script "hljs.highlightAll();")
      ;; MathJax configuration to display inline formulas
      (:script
       "
             MathJax = {
               tex: {
                 inlineMath: [['$','$']],
                 processEscapes: true
               }
             };
        ")
      (:script :type "text/javascript"
               :src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js"))))


(defmethod 40ants-doc/themes/api:render-content ((theme default-theme) uri toc content-func)
  (declare (ignore uri toc))
  (with-html
    (:div :class "content"
          ;; This role is required for Sphinx Doc's
          ;; Javascript code. It searches texts inside
          ;; the role[main] block
          :role "main"
          (when content-func
            (funcall content-func)))))


(defmethod 40ants-doc/themes/api:render-sidebar ((theme default-theme) uri toc)
  (with-html
    (:div :class "sidebar"
          (40ants-doc/themes/api:render-sidebar-header theme uri toc)
          (40ants-doc/themes/api:render-sidebar-content theme uri toc)
          (40ants-doc/themes/api:render-sidebar-footer theme uri toc))))


(defmethod 40ants-doc/themes/api:render-search-form ((theme default-theme) uri toc)
  (with-html
    (:form :method "GET"
           :action (40ants-doc/rewrite::rewrite-url
                    (make-relative-path uri "search/index.html"))
           :class "search"
           (:input :type "text"
                   :name "q")
           (:input :type "submit"
                   :value "Search")
           (:span :id "search-progress"))))


(defmethod 40ants-doc/themes/api:render-toc ((theme default-theme) uri toc)
  (with-html
    (:div :class "page-toc"
          (common-html.emitter::emit toc))))


(defmethod 40ants-doc/themes/api:render-sidebar-header ((theme default-theme) uri toc)
  (with-html
    (:div :class "header"
          (40ants-doc/themes/api:render-search-form theme uri toc))))


(defmethod 40ants-doc/themes/api:render-sidebar-content ((theme default-theme) uri toc)
  (with-html
    (:div :class "content"
          (40ants-doc/themes/api:render-toc theme uri toc))))


(defmethod 40ants-doc/themes/api:render-sidebar-footer ((theme default-theme) uri toc)
  (declare (ignore uri toc))
  (with-html
    (:div :class "footer"
          (:a :href "https://40ants.com/doc"
              "[generated by 40ANTS-DOC]"))))
