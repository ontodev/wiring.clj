(ns wiring.ldtab2ofn.expressionTranslation.propertyTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.ldtab2ofn.util :as util]
            [wiring.ldtab2ofn.spec :as owlspec]))

(declare translate) ;recursive parsing (not tail recursive)  

;TODO put this in util?
(defn curry-predicate-map
  [predicateMap]
  (fn [x] (-> predicateMap
              (get x)
              first
              :object)))

;;NB: these are lists for property expressions (due to the local recursive call)
;TODO: why is this here? 
(defn translateList
  "Translate an RDF list."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/list predicates)]
   ;:post [(spec/valid? string? %)]}
  (loop [in predicates
         out []]
    (if (= in "rdf:nil")
      out
      (recur (:object (first (:rdf:rest in)))
             (conj out (translate (:object (first (:rdf:first in)))))))));recursively translate property expressions

(defn translateInverseOf
  "Translate the inverse of a property"
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/inverseOf predicates)]}
  (let [get-object (curry-predicate-map predicates)
        property (translate (get-object :owl:inverseOf))];you can nest inverses..
    (vector "ObjectInverseOf" property)))

(defn translate
  "Translate property map containing a property expression to OFS."
  [predicateMap]
  (if (string? predicateMap)
    predicateMap ;base case 
    (translateInverseOf predicateMap)));recursion (there is no rdf:type to check for)
