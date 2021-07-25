(ns wiring.thick2ofn.expressionTranslation.propertyTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.util :as util]
            [wiring.thick2ofn.spec :as owlspec]))

(declare translate) ;recursive parsing (not tail recursive)  

;;NB: these are lists for property expressions (due to the local recursive call)
(defn translateList
  "Translate an RDF list."
  [predicates]
  {:pre [(spec/valid? ::owlspec/list predicates)]
   :post [(spec/valid? string? %)]}
  (loop [in predicates
         out []]
    (if (= in "rdf:nil")
      (apply util/ofsFormat out)
      (recur (:rdf:rest in)
             (conj out (translate (:rdf:first in)))))));recursively translate property expressions

(defn translateInverseOf
  "Translate the inverse of a property"
  [predicates]
  {:pre [(spec/valid? ::owlspec/inverseOf predicates)]
   ;:post [(spec/valid? string? %)]
   }
  (let [property (translate (:owl:inverseOf predicates))];you can nest inverses..
    (vector "ObjectInverseOf" property)))

(defn translate
  "Translate property map containing a property expression to OFS."
  [predicateMap]
  (if (string? predicateMap)
    predicateMap ;base case 
    (translateInverseOf predicateMap)));recursion (there is no rdf:type to check for)
