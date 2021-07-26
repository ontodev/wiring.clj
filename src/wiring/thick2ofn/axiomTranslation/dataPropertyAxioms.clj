(ns wiring.thick2ofn.axiomTranslation.dataPropertyAxioms
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [wiring.thick2ofn.expressionTranslation.classTranslation :as CET]
            [wiring.thick2ofn.dataPropertyTranslation :as DPT]
            [wiring.thick2ofn.dataTypeTranslation :as DTT]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.util :as util]
            [wiring.thick2ofn.spec :as owlspec]))

;TODO data validation
;TODO equivalent data properties

(declare translate) 

(defn translateSubDataPropertyOf
  "Translate subPropertyOf"
  [predicates]
  (let [subProperty (DPT/translate (:subject predicates)) 
        superProperty (DPT/translate (:object predicates))]
    (vector "SubDataPropertyOf" subProperty superProperty)))

(defn translateDisjointProperties
  "Translate propertyDisjointWith"
  [predicates]
  (let [lhs (DPT/translate (:subject predicates))
        rhs (DPT/translate (:object predicates))]
    (vector "DisjointDataProperties" lhs rhs)))

(defn translateDomain
  "Translate rdfs:domain"
  [predicates]
  (let [property (DPT/translate (:subject predicates))
        domain (CET/translate (:object predicates))]
    (vector "DataPropertyDomain" property domain)))

(defn translateRange
  "Translate rdfs:range"
  [predicates]
  (let [property (DPT/translate (:subject predicates))
        rangeClass (DTT/translate (:object predicates))]
    (vector "DataPropertyRange" property rangeClass)))

(defn translateFunctionalProperty
  "Translate owl:FunctionalProperty"
  [predicates]
  (let [property (DPT/translate (:subject predicates))]
    (vector "FunctionalDataProperty" property)))

(defn translateType
  "Translate rdf:type for axioms"
  [predicateMap]
  (let [object (:object predicateMap)]
    (case object 
      "owl:FunctionalProperty" (translateFunctionalProperty predicateMap))))

;TODO: all n-ary disjoint properties
(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (let [p (:predicate predicateMap)];
    (case p 
      "owl:propertyDisjointWith" (translateDisjointProperties predicateMap)
      "rdfs:domain" (translateDomain predicateMap)
      "rdfs:range" (translateRange predicateMap)
      "rdfs:subPropertyOf" (translateSubDataPropertyOf predicateMap)

      "rdf:type" (translateType predicateMap))))
