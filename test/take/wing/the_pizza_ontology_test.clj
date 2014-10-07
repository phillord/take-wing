(ns take.wing.the-pizza-ontology-test
  (:use [clojure.test])
  (:require [take.wing.the-pizza-ontology :as p]
            [tawny.owl :as o]
            [tawny.reasoner :as r]
            [tawny.fixture]))

(use-fixtures :once
  (tawny.fixture/namespace-and-reasoner
   'take.wing.the-pizza-ontology
   :hermit))

(deftest loading []
  (is true))

(deftest consistent []
  (is (r/coherent?))
  (is (r/consistent?)))

(deftest marg
  (is (o/subclass? p/Pizza p/MargheritaPizza)))

(deftest pizza-subclasses
  (is
   (= #{}
      (clojure.set/difference
       #{p/HawaiianPizza p/MargheritaPizza}
       (o/subclasses p/Pizza)))))


(deftest marg-veggie
  (is
   (not
    (o/subclass? p/VegetarianPizza p/MargheritaPizza)))
  )
