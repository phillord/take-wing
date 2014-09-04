(println "Running gensource")

(use 'clojure.java.shell)


(println
 (:err
  (sh "cask" "exec" "emacs" "--script" "script/gensource.el")))

(println "Running gensource complete")
