(ns wiring.ofn2ldtab.expressionTranslation.propertyTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2ldtab.spec :as owlspec])
  (:gen-class))

;TODO data validation
(declare translate)

(defn translateInverseOf
  "Translate ObjectInverseOf expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/inverseOf ofn)]}
  (let [[op arg] ofn
        triple {:owl:inverseOf [{:object (translate arg) }]}]
    triple))

(defn translate
  "Translate OFN-S property expression into predicate map"
  [ofn]
  ;(println ofn)
  (if (= "ObjectInverseOf" (first ofn))
    (translateInverseOf ofn)
    ofn)) 
