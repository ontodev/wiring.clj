(ns wiring.ofn2ldtab.expressionTranslation.propertyTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2ldtab.util :as u]
            [wiring.ofn2ldtab.spec :as owlspec])
  (:gen-class))

;TODO data validation
(declare translate)

(defn translate-object
  [object]
  [{:object (translate object) :datatype (u/translate-datatype object)}]) 

(defn translateList
  "Translate property expressions into an RDF list"
  [expressions]
  (loop [in (reverse expressions);constructing list from last element to first
         out "rdf:nil"]
    (if (empty? in)
      out
      (recur (rest in)
             {:rdf:first (translate-object (first in))
              :rdf:rest (translate-object out)}))))

(defn translatePropertyChain
  "Translate ObjectInverseOf expression"
  [ofn]
  (let [[_op & args] ofn]
    (translateList args)))

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
  (cond
    (= "ObjectInverseOf" (first ofn))
    (translateInverseOf ofn)
    (= "ObjectPropertyChain" (first ofn))
    (translatePropertyChain ofn)
    :else ofn)) 
