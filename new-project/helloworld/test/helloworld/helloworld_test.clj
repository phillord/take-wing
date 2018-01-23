(ns helloworld.helloworld-test
    (:use [clojure.test])
    (:require
     [helloworld.helloworld :as ont]
     [tawny.owl :as o]
     [tawny.reasoner :as r]
     [tawny.fixture :as f]))

(use-fixtures :each (f/reasoner :hermit))

(deftest reasonable
  (is (r/consistent? helloworld.helloworld/helloworld))
  (is (r/coherent? helloworld.helloworld/helloworld)))
