(ns take.wing.amino-acid-test
  (:use [clojure.test])
  (:require
   [tawny.fixture]
   [tawny.reasoner :as r]
   [take.wing.amino-acid :as a]))

(use-fixtures :once
  (tawny.fixture/namespace-and-reasoner
   'take.wing.amino-acid
   :hermit))

(deftest consistent []
  (is (r/coherent?))
  (is (r/consistent?)))


