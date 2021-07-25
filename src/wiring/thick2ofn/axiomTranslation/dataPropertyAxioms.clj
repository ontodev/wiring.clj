(ns wiring.thick2ofn.axiomTranslation.dataPropertyAxioms
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [wiring.thick2ofn.dataPropertyTranslation :as DPT]
            [wiring.thick2ofn.dataTypeTranslation :as DTT]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.util :as util]
            [wiring.thick2ofn.spec :as owlspec]))

;TODO data validation
(declare translate)

;TODO equivalent data properties

(defn translateSubObjectPropertyOf
  "Translate subPropertyOf"
  [predicates]
  (let [subProperty (DPT/translate (:subject predicates)) 
        superProperty (DPT/translate (:object predicates))]
    (util/ofsFormat "SubDataPropertyOf" subProperty superProperty)))

(defn translateDisjointProperties
  "Translate propertyDisjointWith"
  [predicates]
  (let [lhs (DPT/translate (:subject predicates))
        rhs (DPT/translate (:object predicates))]
    (util/ofsFormat "DisjointObjectProperties" lhs rhs)))

(defn translateDomain
  "Translate rdfs:domain"
  [predicates]
  (let [property (DPT/translate (:subject predicates))
        domain (DTT/translate (:object predicates))]
    (util/ofsFormat "ObjectPropertyDomain" property domain)))

(defn translateRange
  "Translate rdfs:range"
  [predicates]
  (let [property (DPT/translate (:subject predicates))
        rangeClass (DTT/translate (:object predicates))]
    (util/ofsFormat "ObjectPropertyRange" property rangeClass)))

(defn translateFunctionalProperty
  "Translate owl:FunctionalProperty"
  [predicates]
  (let [property (DPT/translate (:subject predicates))]
    (util/ofsFormat "FunctionalObjectProperty" property)))
