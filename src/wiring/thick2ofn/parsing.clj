(ns wiring.thick2ofn.parsing
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.spec :as owlspec]))

;TODO handle single quotation marks {'subject': 'ex:A', 'predicate': 'rdf:type', 'object': 'owl:Class'} 
;TODO: handle quotations around thick map after 'object' 

(defn parse
  "Parse JSON predicate map."
  [predicateMap]
  {:pre [(spec/valid? string? predicateMap)] ;a 'map' can be just a string, i.e. a normal object of a triple
   :post [(spec/valid? ::owlspec/map %)]}
  (let [removedObject (s/replace predicateMap #"\[\{\"object\":" "")
        removedClosingBracket (s/replace removedObject  #"\}\]" "")
        predicates (cs/parse-string removedClosingBracket true)]
    predicates))
