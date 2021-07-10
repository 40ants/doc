(defpackage #:40ants-doc/themes/default
  (:use #:cl)
  (:import-from #:40ants-doc/themes/api
                #:render-css)
  (:import-from #:lass)
  (:export
   #:default-theme))
(in-package 40ants-doc/themes/default)


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
      :padding 0.1em
      :border "solid 1px"
      :font-weight bold
      (a
       :border-bottom none))

     (.locative-args
      :font-style italic
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


     ;; Syntax highlighting with Colorize
     (.symbol
      :color "#770055"
      :background-color transparent
      :text-decoration none
      :border 0px
      :margin 0px)
     ((:and a.symbol :link)
      :color "#229955")
     ((:and a.symbol :active)
      :color "#229955")
     ((:and a.symbol :visited)
      :color "#229955")
     ((:and a.symbol :hover)
      :color "#229955")
     (.special
      :color "#FF5000"
      :background-color inherit)
     (.keyword
      :color "#770000"
      :background-color inherit)
     (.comment
      :color "#007777"
      :background-color inherit)
     (.string
      :color "#777777"
      :background-color inherit)
     (.atom
      :color "#314F4F"
      :background-color inherit)
     (.macro
      :color "#FF5000"
      :background-color inherit)
     (.variable
      :color "#36648B"
      :background-color inherit)
     (.function
      :color "#8B4789"
      :background-color inherit)
     (.attribute
      :color "#FF5000"
      :background-color inherit)
     (.character
      :color "#0055AA"
      :background-color inherit)
     (.syntaxerror
      :color "#FF0000"
      :background-color inherit)
     (.diff-deleted
      :color "#5F2121"
      :background-color inherit)
     (.diff-added
      :color "#215F21"
      :background-color inherit)


     ;; Content
     (|#content-container|
      :margin 0
      :padding 0)

     (|#content|
      :margin-left 40ex
      :padding-left 2.5em
      :max-width 85ex)

     ;; Toc bar
     (|#toc|
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
      (ul 
       :font-size 80%
       :margin 0
       :padding 0
       :list-style none)
      (li
       :line-height 1.0
       :padding "5px 10px")
      (a
       :border-bottom none)
      (hr
       :height 0.05em
       :border 0
       :background "#777")
      (.toc-h2
       :padding-left 10px)
      (.toc-h3 
       :padding-left 20px)
      (.toc-h4 
       :padding-left 30px)
      (.toc-active 
       :background "#336699"
       :box-shadow inset -5px 0px 10px -5px "#000"))

     (|#page-toc|
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

     (|#toc-header|
      (a
       :color "#777777"))

     (|#toc-footer|
      :text-align center
      (a
       :font-size 80%
       :color "#777777")))))