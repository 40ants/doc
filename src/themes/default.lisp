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
     (|#content-container|
      :margin 0
      :padding 0)

     (|#content|
      :margin-left 40ex
      :padding-left 2.5em
      :max-width 85ex)

     ;; Side-bar

     (form.search
      :margin-left 1.5em
      :margin-top 1.5em)
     
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
      :margin-left 1.5em
      :margin-top 2em
      (a
       :font-size 80%
       :color "#777777"))

     (|#search-results|
      (.search
       (li
        :margin-bottom 1em)))

     (.unresolved-reference
      :color magenta))))
