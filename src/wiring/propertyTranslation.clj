(ns wiring.propertyTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.util :as util]
            [wiring.spec :as owlspec]))

(declare translate) ;recursive parsing (not tail recursive)  

(defn translateInverseOf
  "Translate the inverse of a property"  
  [predicates]
  {:pre [(spec/valid? ::owlspec/inverseOf predicates)]
   :post [(spec/valid? string? %)]}
  (let [property (translate (:owl:inverseOf predicates))];you can nest inverses..
    (util/ofsFormat "ObjectInverseOf" property))) 

(defn translate
  "Translate property map containing a property expression to OFS."
  [predicateMap]
  (if (string? predicateMap)
    predicateMap ;base case 
    (translateInverseOf predicateMap)));recursion (there is no rdf:type to check for)
