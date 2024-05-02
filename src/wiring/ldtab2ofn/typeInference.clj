(ns wiring.ldtab2ofn.typeInference
  (:require [clojure.repl :as repl]
            [clojure.string :as s]))

(defn is-class-expression?
  "Checks whether an expression is a non-atomic OWL class expression."
  [expression]
  (case (first expression)
    "ObjectSomeValuesFrom" true
    "ObjectAllValuesFrom" true
    "ObjectHasValue" true
    "ObjectMinCardinality" true
    "ObjectMaxCardinality" true
    "ObjectExactCardinality" true
    "ObjectIntersectionOf" true
    "ObjectUnionOf" true
    "ObjectOneOf" true
    "ObjectComplementOf" true
    "ObjectHasSelf" true
      ;ambiguous expressions
    "SomeValuesFrom" true
    "AllValuesFrom" true
    "HasValue" true
    "MaxCardinality" true
    "MinCardinality" true
    "ExactCardinality" true
    false))

(defn is-typed-as-class?
  "Checks whether a predicate map is typed as an OWL class expression."
  [expression]
  (if (and (map? expression)
           (contains? expression :rdf:type))
    (or (= "owl:Class" (:rdf:type expression))
        (= "owl:Restriction" (:rdf:type expression)));Restrictions are class expressions
    false));note that this only checks information contained within a predicate map 

(defn is-data-range-expression?
  "Checks whether an expression is a non-atomic OWL datatype expression."
  [expression]
  (case (first expression)
    "DataIntersectionOf" true
    "DataUnionOf" true
    "DataComplementOf" true
    "DatatypeRestriction" true
    "DataOneOf" true
    false))

(defn is-typed-as-datatype?
  "Checks whether input 'expression' is a datatype expression."
  [expression]
  (and (map? expression)
       (contains? expression :rdf:type)
       (= "rdfs:Datatype" (:rdf:type expression))))

(defn is-object-property-expression?
  "Checks whether an expression is a non-atomic OWL object property expression."
  [expression]
  (case (first expression)
    "ObjectInverseOf" true
    false))

(defn is-ambiguous-expression?
  "Checks whether an expression is an ambiguous OWL class expression/data range"
  [expression]
  (case (first expression)
    "IntersectionOf" true
    "UnionOf" true
    "ComplementOf" true
    "OneOf" true
    false))
