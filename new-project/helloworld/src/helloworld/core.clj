(ns helloworld.core
  [:use [tawny.owl]]
  [:require [helloworld.helloworld]])


(defn -main [& args]
  (save-ontology helloworld.helloworld/helloworld "helloworld.omn"))
