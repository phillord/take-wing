(defproject helloworld "0.0.1-SNAPSHOT"
  :description "An ontology for helloworld"
  :dependencies [[uk.org.russet/tawny-owl "2.0.0-SNAPSHOT"]]
  :main helloworld.core

  :profiles
  {:light {:plugins [[nightlight/lein-nightlight "1.9.0"]]}}
  )
