(ns wiring.ofn2thick.classTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2thick.propertyTranslation :as property]
            [wiring.ofn2thick.spec :as owlspec])
  (:gen-class))

(declare translate)

(defn translateList
  "Translate class expressions into an RDF list"
  [expressions]
  {:pre [(spec/valid? ::owlspec/list expressions)]}
  (loop [in (reverse expressions);constructing list from last element to first
         out "rdf:nil"]
    (if (empty? in)
      out
      (recur (rest in)
             {:rdf:first [{:object (translate (first in)) }]
              :rdf:rest [{:object out}]}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Restrictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateObjectSomeValuesFrom
  "Translate a ObjectSomeValuesFrom expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/someValuesFrom ofn)]}
  (let [[op property filler] ofn
        triple {:owl:someValuesFrom [{:object (translate filler) }]
                :owl:onProperty [{:object (property/translate property) }]
                :rdf:type [{:object "owl:Restriction"}]}]
    triple))

(defn translateObjectAllValuesFrom
  "Translate a ObjectAllValuesFrom expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/allValuesFrom ofn)]}
  (let [[op property filler] ofn
        triple {:owl:allValuesFrom [{:object (translate filler) }]
                :owl:onProperty [{:object (property/translate property) }]
                :rdf:type [{:object "owl:Restriction"}]}]
    triple))

(defn translateObjectHasValue
  "Translate a ObjectHasValue expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/hasValue ofn)]}
  (let [[op property filler] ofn
        triple {:owl:hasValue [{:object (translate filler) }]
                :owl:onProperty [{:object (property/translate property) }]
                :rdf:type [{:object "owl:Restriction"}]}]
    triple))

(defn translateObjectHasSelf
  "Translate a ObjectHasValue expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/hasSelf ofn)]}
  (let [[op property] ofn
        triple {:owl:hasSelf [{:object "true^^xsd:boolean"}]
                :owl:onProperty [{:object (property/translate property) }]
                :rdf:type [{:object "owl:Restriction" }]}]
    triple))

(defn translateObjectMinUnqualifiedCardinality
  "Translate a ObjectMinCardinality expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/minCardinality ofn)]}
  (let [[op cardinality property] ofn
        triple {:owl:minCardinality [{:object (str cardinality "^^xsd:nonNegativeInteger") }]
                :owl:onProperty [{:object (property/translate property) }]
                :rdf:type [{:object "owl:Restriction"}]}]
    triple))

(defn translateObjectMinQualifiedCardinality
  "Translate a ObjectMinQualifiedCardinality expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/minQualifiedCardinality ofn)]}
  (let [[op cardinality property filler] ofn
        triple {:owl:minQualifiedCardinality [{:object (str cardinality "^^xsd:nonNegativeInteger")}]
                :owl:onProperty [{:object (property/translate property) }]
                :owl:onClass [{:object (translate filler) }]
                :rdf:type [{:object "owl:Restriction" }]}]
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
        triple {:owl:maxCardinality [{:object (str cardinality "^^xsd:nonNegativeInteger") }]
                :owl:onProperty [{:object (property/translate property) }]
                :rdf:type [{:object "owl:Restriction"}]}]
    triple))

(defn translateObjectMaxQualifiedCardinality
  "Translate a ObjectMaxQualifiedCardinality expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/maxQualifiedCardinality ofn)]}
  (let [[op cardinality property filler] ofn
        triple {:owl:maxQualifiedCardinality [{:object (str cardinality "^^xsd:nonNegativeInteger")}]
                :owl:onProperty [{:object (property/translate property)}]
                :owl:onClass[{:object (translate filler) }]
                :rdf:type [{:object "owl:Restriction"}]}]
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
        triple {:owl:cardinality [{:object (str cardinality "^^xsd:nonNegativeInteger")}]
                :owl:onProperty [{:object (property/translate property) }]
                :rdf:type [{:object "owl:Restriction"}]}]
    triple))

(defn translateObjectExactQualifiedCardinality
  "Translate a ObjectExactQualifiedCardinality expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/exactQualifiedCardinality ofn)]}
  (let [[op cardinality property filler] ofn
        triple {:owl:qualifiedCardinality [{:object (str cardinality "^^xsd:nonNegativeInteger") }]
                :owl:onProperty [{:object (property/translate property) }]
                :owl:onClass [{:object (translate filler) }]
                :rdf:type [{:object "owl:Restriction"}]}]
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
        triple {:owl:intersectionOf [{:object (translateList arguments) }]
                :rdf:type [{:object "owl:Class" }]}]
    triple)) 

(defn translateObjectUnion
  "Translate an ObjectIntersectionOf expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/classUnion ofn)]}
  (let [[operator & arguments] ofn
        triple {:owl:unionOf [{:object (translateList arguments)}]
                :rdf:type [{:object "owl:Class" }]}]
    triple))

(defn translateObjectOneOf
  "Translate an ObjectOneOf expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/oneOf ofn)]}
  (let [[operator & arguments] ofn
        triple {:owl:oneOf [{:object (translateList arguments) }]
                :rdf:type [{:object "owl:Class"}]}]
    triple))

(defn translateObjectComplement
  "Translate an ObjectComplementOf expression"
  [ofn]
  {:pre [(spec/valid? ::owlspec/classComplement ofn)]}
  (let [[operator argument] ofn
        triple {:owl:complementOf [{:object (translate argument) }]
                :rdf:type [{:object "owl:Class"}]}]
    triple))

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
      ofn)))

