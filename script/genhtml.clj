(println "Running genhtml")

(use 'clojure.java.shell)

(println
 (:err
  (sh "asciidoc" "book.asciidoc")))

(println "Running genhtml complete")
