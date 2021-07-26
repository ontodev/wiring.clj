(ns wiring.thick2ofn.axiomTranslation.dataPropertyAxioms
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [wiring.thick2ofn.expressionTranslation.classTranslation :as CET]
            [wiring.thick2ofn.expressionTranslation.dataPropertyTranslation :as DPT]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.util :as util]
            [wiring.thick2ofn.spec :as owlspec]))

;TODO data validation
;TODO equivalent data properties

(declare translate) 

(defn translateList
  "Translate an RDF list."
  [predicates]
  {:pre [(spec/valid? ::owlspec/list predicates)]}
  (loop [in predicates
         out []]
    (if (= in "rdf:nil")
      out
      (recur (:rdf:rest in)
             (conj out (translate (:rdf:first in)))))));recursively translate property expressions

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

(defn translateAllDisjointProperties 
  "Translate owl:AllDisjointProperties"
  [predicates]
  (let [arguments (DPT/translateList (:owl:members (:object predicates)))]
    (vec (cons "DisjointDataProperties" arguments))))

(defn translateType
  "Translate rdf:type for axioms"
  [predicateMap]
  (let [object (:object predicateMap)]
    (case object 
      "owl:FunctionalProperty" (translateFunctionalProperty predicateMap)
      "owl:AllDisjointProperties" (translateAllDisjointProperties predicateMap))))

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
