(ns wiring.ofn2ldtab.expressionTranslation.classTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            ;TODO remove property dependency
            [wiring.ofn2ldtab.expressionTranslation.propertyTranslation :as property]
            [wiring.ofn2ldtab.util :as u]
            [wiring.ofn2ldtab.spec :as owlspec])
  (:gen-class))

(declare translate)

(defn translate-object
  [object]
  [{:object (translate object) :datatype (u/translate-datatype object)}]) 

(defn translateList
  "Translate class expressions into an RDF list"
  [expressions]
  {:pre [(spec/valid? ::owlspec/list expressions)]}
  (loop [in (reverse expressions);constructing list from last element to first
         out "rdf:nil"]
    (if (empty? in)
      out
      (recur (rest in)
             {:rdf:first (translate-object (first in))
              :rdf:rest (translate-object out)})))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Restrictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateObjectSomeValuesFrom
  "Translate a ObjectSomeValuesFrom expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/someValuesFrom ofn)]}
  (let [[op property filler] ofn
        triple {:owl:someValuesFrom (translate-object filler)
                :owl:onProperty (translate-object property)
                :rdf:type (translate-object "owl:Restriction")}]
    triple))

(defn translateObjectAllValuesFrom
  "Translate a ObjectAllValuesFrom expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/allValuesFrom ofn)]}
  (let [[op property filler] ofn
        triple {:owl:allValuesFrom (translate-object filler)
                :owl:onProperty (translate-object property) 
                :rdf:type (translate-object "owl:Restriction")}]
    triple))

(defn translateObjectHasValue
  "Translate a ObjectHasValue expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/hasValue ofn)]}
  (let [[op property filler] ofn
        triple {:owl:hasValue (translate-object filler)
                :owl:onProperty (translate-object property)
                :rdf:type (translate-object "owl:Restriction")}]
    triple))

(defn translateObjectHasSelf
  "Translate a ObjectHasValue expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/hasSelf ofn)]}
  (let [[op property] ofn
        ;TODO test use of 'translate-object' for "true^^xsd:boolean"
        triple {:owl:hasSelf (translate-object "true^^xsd:boolean")
                :owl:onProperty (translate-object property)
                :rdf:type (translate-object "owl:Restriction") }]
    triple))

(defn translateObjectMinUnqualifiedCardinality
  "Translate a ObjectMinCardinality expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/minCardinality ofn)]}
  (let [[op cardinality property] ofn
        triple {:owl:minCardinality (translate-object (str cardinality "^^xsd:nonNegativeInteger") )
                :owl:onProperty (translate-object property)
                :rdf:type (translate-object "owl:Restriction")}]
    triple))

(defn translateObjectMinQualifiedCardinality
  "Translate a ObjectMinQualifiedCardinality expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/minQualifiedCardinality ofn)]}
  (let [[op cardinality property filler] ofn
        triple {:owl:minQualifiedCardinality (translate-object (str cardinality "^^xsd:nonNegativeInteger"))
                :owl:onProperty (translate-object property)
                :owl:onClass (translate-object filler)
                :rdf:type (translate-object "owl:Restriction") }]
    triple))

(defn translateObjectMinCardinality
  "Translate a ObjectMinCardinality expression"
  [ofn]
  (if (= 3 (count ofn))
    (translateObjectMinUnqualifiedCardinality ofn)
    (translateObjectMinQualifiedCardinality ofn)))

(defn translateObjectMaxUnqualifiedCardinality
  "Translate a ObjectMaxCardinality expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/maxCardinality ofn)]}
  (let [[op cardinality property] ofn
        triple {:owl:maxCardinality (translate-object (str cardinality "^^xsd:nonNegativeInteger"))
                :owl:onProperty (translate-object property)
                :rdf:type (translate-object "owl:Restriction")}]
    triple))

(defn translateObjectMaxQualifiedCardinality
  "Translate a ObjectMaxQualifiedCardinality expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/maxQualifiedCardinality ofn)]}
  (let [[op cardinality property filler] ofn
        triple {:owl:maxQualifiedCardinality (translate-object (str cardinality "^^xsd:nonNegativeInteger"))
                :owl:onProperty (translate-object property)
                :owl:onClass (translate-object filler) 
                :rdf:type (translate-object "owl:Restriction")}]
    triple))

(defn translateObjectMaxCardinality
  "Translate a ObjectMaxCardinality expression"
  [ofn]
  (if (= 3 (count ofn))
    (translateObjectMaxUnqualifiedCardinality ofn)
    (translateObjectMaxQualifiedCardinality ofn)))

(defn translateObjectExactUnqualifiedCardinality
  "Translate a ObjectExactCardinality expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/exactCardinality ofn)]}
  (let [[op cardinality property] ofn
        triple {:owl:cardinality (translate-object (str cardinality "^^xsd:nonNegativeInteger"))
                :owl:onProperty (translate-object (property/translate property))
                :rdf:type (translate-object "owl:Restriction")}]
    triple))

(defn translateObjectExactQualifiedCardinality
  "Translate a ObjectExactQualifiedCardinality expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/exactQualifiedCardinality ofn)]}
  (let [[op cardinality property filler] ofn
        triple {:owl:qualifiedCardinality (translate-object (str cardinality "^^xsd:nonNegativeInteger"))
                :owl:onProperty (translate-object (property/translate property))
                :owl:onClass (translate-object (translate filler))
                :rdf:type (translate-object "owl:Restriction")}]
    triple)) 

(defn translateObjectExactCardinality
  "Translate a ObjectExactCardinality expression"
  [ofn]
  (if (= 3 (count ofn))
    (translateObjectExactUnqualifiedCardinality ofn)
    (translateObjectExactQualifiedCardinality ofn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Propositional Connectives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateObjectIntersection
  "Translate an ObjectIntersectionOf expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/classIntersection ofn)]}
  (let [[operator & arguments] ofn
        triple {:owl:intersectionOf (translate-object (translateList arguments))
                :rdf:type (translate-object "owl:Class")}]
    triple)) 

(defn translateObjectUnion
  "Translate an ObjectIntersectionOf expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/classUnion ofn)]}
  (let [[operator & arguments] ofn
        triple {:owl:unionOf (translate-object (translateList arguments))
                :rdf:type (translate-object "owl:Class") }]
    triple))

(defn translateObjectOneOf
  "Translate an ObjectOneOf expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/oneOf ofn)]}
  (let [[operator & arguments] ofn
        triple {:owl:oneOf (translate-object (translateList arguments))
                :rdf:type (translate-object "owl:Class")}]
    triple))

(defn translateObjectComplement
  "Translate an ObjectComplementOf expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/classComplement ofn)]}
  (let [[operator argument] ofn
        triple {:owl:complementOf (translate-object (translate argument))
                :rdf:type (translate-object "owl:Class")}]
    triple))

(defn translateInverseOf
  "Translate ObjectInverseOf expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/inverseOf ofn)]}
  (let [[op arg] ofn
        triple {:owl:inverseOf (translate-object arg) }]
    triple))

(defn translate-string
  [string]
  (let [datatype (u/get-datetype string)]
    (println string)
    (case datatype
      "URI" string
      "CURIE" string
      "LITERAL" (second (re-matches #"^\"(.+)\"(.*)$" string))
      "JSON" string)))

(defn translate
  "Translate OFN-S expression to tick triple"
  [ofn]
  (let [operator (first ofn)];
    (case operator
      "ObjectSomeValuesFrom"  (translateObjectSomeValuesFrom ofn)
      "ObjectAllValuesFrom"  (translateObjectAllValuesFrom ofn)
      "ObjectHasValue"  (translateObjectHasValue ofn)
      "ObjectMinCardinality"  (translateObjectMinCardinality ofn)
      "ObjectMaxCardinality"  (translateObjectMaxCardinality ofn)
      "ObjectExactCardinality"  (translateObjectExactCardinality ofn)
      "ObjectIntersectionOf"  (translateObjectIntersection ofn)
      "ObjectUnionOf"  (translateObjectUnion ofn)
      "ObjectOneOf"  (translateObjectOneOf ofn)
      "ObjectComplementOf"  (translateObjectComplement ofn)
      "ObjectHasSelf"  (translateObjectHasSelf ofn)
      "ObjectInverseOf" (translateInverseOf ofn)
      ;translate ambiguous OFN-S expressions
      ;note that the translation for ambiguous OFN-S expressions is
      ;the same
      "SomeValuesFrom" (translateObjectSomeValuesFrom ofn)
      "AllValuesFrom" (translateObjectAllValuesFrom ofn)
      "HasValue" (translateObjectHasValue ofn)
      "MaxCardinality" (translateObjectMaxCardinality ofn)
      "MinCardinality" (translateObjectMinCardinality ofn)
      "ExactCardinality" (translateObjectExactCardinality ofn)
      "OneOf" (translateObjectOneOf ofn)
      (translate-string ofn))))

