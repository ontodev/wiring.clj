(ns wiring.thick2ofn.axiomTranslation.objectPropertyAxioms
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.util :as util]
            [wiring.thick2ofn.propertyTranslation :as propertyTranslation]
            [wiring.thick2ofn.classTranslation :as classTranslation]
            [wiring.thick2ofn.spec :as owlspec]))

;TODO data validation

;TODO equivalent data properties

(declare translate)

(defn translateSubObjectPropertyOf
  "Translate subPropertyOf"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        superProperty (propertyTranslation/translate (:object predicates))]
    (util/ofsFormat "SubObjectPropertyOf" subProperty superProperty)))

(defn translatePropertyChainAxiom
  "Translate propertyChainAxiom"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        propertyChain (propertyTranslation/translate (:object predicates))]
    (util/ofsFormat "SubObjectPropertyOf" (util/ofsFormat "ObjectPropertyChain" propertyChain subProperty))))

(defn translateDisjointProperties
  "Translate propertyDisjointWith"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        superProperty (propertyTranslation/translate (:object predicates))]
    (util/ofsFormat "DisjointObjectProperties" subProperty superProperty)))

(defn translateDomain
  "Translate rdfs:domain"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))
        domain (classTranslation/translate (:object predicates))]
    (util/ofsFormat "ObjectPropertyDomain" property domain)))

(defn translateRange
  "Translate rdfs:range"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))
        rangeClass (classTranslation/translate (:object predicates))]
    (util/ofsFormat "ObjectPropertyRange" property rangeClass)))

(defn translateInverseOf
  "Translate owl:inverseOf"
  [predicates]
  (let [property1 (propertyTranslation/translate (:subject predicates))
        property2 (propertyTranslation/translate (:object predicates))]
    (util/ofsFormat "InverseObjectProperties" property1 property2)))

;;NB these things all follow the same structure - should they be refactored into a single function?
;(defn translateThinPropertyAxiom
;  "Translate thin property axioms"
;  [predicates description]
;  (let [property (propertyTranslation/translate (:subject predicates))]
;    (util/ofsFormat description property)))

;;hardcoding things
(defn translateFunctionalProperty
  "Translate owl:FunctionalProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (util/ofsFormat "FunctionalObjectProperty" property)))

(defn translateInverseFunctionalProperty
  "Translate owl:InverseFunctionalProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (util/ofsFormat "InverseFunctionalObjectProperty" property)))

(defn translateReflexiveProperty
  "Translate owl:ReflexiveProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (util/ofsFormat "ReflexiveObjectProperty" property)))

(defn translateIrreflexiveProperty
  "Translate owl:IrreflexiveProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (util/ofsFormat "IrreflexiveObjectProperty" property)))

(defn translateAsymmetricProperty
  "Translate owl:AsymmetricProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (util/ofsFormat "AsymmetricObjectProperty" property)))

(defn translateSymmetricProperty
  "Translate owl:SymmetricProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (util/ofsFormat "SymmetricObjectProperty" property)))

(defn translateTransitiveProperty
  "Translate owl:TransitiveProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (util/ofsFormat "TransitiveObjectProperty" property)))

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (let [p (:predicate predicateMap)];
    ;(println predicateMap)
    ;(println p) 
    (case p
      "rdfs:subPropertyOf" (translateSubObjectPropertyOf predicateMap)
      "owl:propertyChainAxiom" (translateSubObjectPropertyOf predicateMap)
      "owl:propertyDisjointWith" (translateDisjointProperties predicateMap)
      "rdfs:domain" (translateDomain predicateMap)
      "rdfs:range" (translateRange predicateMap)
      "owl:inverseOf" (translateInverseOf predicateMap)
      "owl:InverseFunctionalProperty" (translateInverseFunctionalProperty predicateMap)
      "owl:ReflexiveProperty" (translateReflexiveProperty predicateMap) 
      "owl:IrreflexiveProperty" (translateIrreflexiveProperty predicateMap) 
      "owl:AsymmetricProperty" (translateAsymmetricProperty predicateMap) 
      "owl:SymmetricProperty" (translateSymmetricProperty predicateMap) 
      "owl:TransitiveProperty" (translateTransitiveProperty predicateMap)

      )))


