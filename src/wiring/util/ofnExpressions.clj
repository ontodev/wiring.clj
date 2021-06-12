(ns wiring.util.ofnExpressions
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]))

;assume an OFN expression to be parsed already
;in that case, an expression has to be a named one,
;if it is *not* of the form [operator & args], i.e. a collection
;but simply a string
(defn namedEntity?
  "Checks whether an expression is named"
  [ofn]
  ;(not (coll? ofn)))
  (string? ofn))
  
  ;(let [operator (first ofn)];
  ;  (case operator
  ;    "ObjectSomeValuesFrom" false
  ;    "ObjectAllValuesFrom" false
  ;    "ObjectHasValue"  false
  ;    "ObjectMinCardinality" false
  ;    "ObjectMaxCardinality" false
  ;    "ObjectExactCardinality" false
  ;    "ObjectIntersectionOf" false
  ;    "ObjectUnionOf" false
  ;    "ObjectOneOf" false
  ;    "ObjectComplementOf" false
  ;    "ObjectInverseOf" false
  ;    "SubClassOf" false
  ;    "DisjointUnion" false
  ;    "DisjointClasses" false
  ;    "EquivalentClasses" false
  ;    ;TODO add all remaining operators (datatypes, etc.)
  ;    ;TODO add numbers
  ;    true)))

