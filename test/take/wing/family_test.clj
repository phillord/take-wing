(ns take.wing.family-test
  (:use [clojure.test])
  (:require [take.wing.family :as f]
            [tawny.owl :as o]
            [tawny.reasoner :as r]
            [tawny.fixture]))

(use-fixtures :once
  (tawny.fixture/namespace-and-reasoner
   'take.wing.family
   :hermit))

(deftest loading []
  (is true))

(deftest consistent []
  (is (r/coherent?))
  (is (r/consistent?)))
