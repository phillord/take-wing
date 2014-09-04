(defproject take-wing "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [uk.org.russet/tawny-owl "1.1.1-SNAPSHOT"]]
  :aliases {"gensource"
            ["exec" "-p" "script/gensource.clj"]
            "html"
            ["exec" "-p" "script/genhtml.clj"]}
  :plugins [[lein-exec "0.3.3"]]
  )
