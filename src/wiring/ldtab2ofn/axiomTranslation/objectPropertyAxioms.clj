(ns wiring.ldtab2ofn.axiomTranslation.objectPropertyAxioms
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.ldtab2ofn.expressionTranslation.propertyTranslation :as propertyTranslation]
            [wiring.ldtab2ofn.expressionTranslation.classTranslation :as classTranslation]
            [wiring.ldtab2ofn.typeInference :as typeInference]
            [wiring.ldtab2ofn.spec :as owlspec]))

;TODO data validation 
;TODO equivalent object properties

(declare translate)

;TODO put this in util?
(defn curry-predicate-map
  [predicateMap]
  (fn [x] (-> predicateMap
              (get x)
              first
              :object)))

(defn translateSubObjectPropertyOf
  "Translate subPropertyOf"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        superProperty (propertyTranslation/translate (:object predicates))]
    (vector "SubObjectPropertyOf" subProperty superProperty)))

(defn translatePropertyChainAxiom
  "Translate propertyChainAxiom"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        propertyChain (propertyTranslation/translateList (:object predicates))]
    (vector "ObjectPropertyChain" propertyChain subProperty)))

(defn translateDisjointProperties
  "Translate propertyDisjointWith"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        superProperty (propertyTranslation/translate (:object predicates))]
    (vector "DisjointObjectProperties" subProperty superProperty)))

(defn translateDomain
  "Translate rdfs:domain"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))
        domain (classTranslation/translate (:object predicates))]
    (vector "ObjectPropertyDomain" property domain)))

(defn translateRange
  "Translate rdfs:range"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))
        rangeClass (classTranslation/translate (:object predicates))]
    (vector "ObjectPropertyRange" property rangeClass)))

(defn translateInverseOf
  "Translate owl:inverseOf"
  [predicates]
  (let [property1 (propertyTranslation/translate (:subject predicates))
        property2 (propertyTranslation/translate (:object predicates))]
    (vector "InverseObjectProperties" property1 property2)))

;;hardcoding things
(defn translateFunctionalProperty
  "Translate owl:FunctionalProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (vector "FunctionalObjectProperty" property)))

(defn translateInverseFunctionalProperty
  "Translate owl:InverseFunctionalProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (vector "InverseFunctionalObjectProperty" property)))

(defn translateReflexiveProperty
  "Translate owl:ReflexiveProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (vector "ReflexiveObjectProperty" property)))

(defn translateIrreflexiveProperty
  "Translate owl:IrreflexiveProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (vector "IrreflexiveObjectProperty" property)))

(defn translateAsymmetricProperty
  "Translate owl:AsymmetricProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (vector "AsymmetricObjectProperty" property)))

(defn translateSymmetricProperty
  "Translate owl:SymmetricProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (vector "SymmetricObjectProperty" property)))

(defn translateTransitiveProperty
  "Translate owl:TransitiveProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (vector "TransitiveObjectProperty" property)))

(defn translateAllDisjointProperties
  "Translate owl:AllDisjointProperties"
  [predicates]
  (let [arguments (propertyTranslation/translateList (:owl:members (:object predicates)))]
    (if (some typeInference/is-object-property-expression? arguments)
      (vec (cons "DisjointObjectProperties" arguments))
      (vec (cons "DisjointProperties" arguments)))))

(defn translateEquivalentProperty
  "Translate owl:equivalentProperty"
  [predicates]
  (let [lhs (propertyTranslation/translate (:subject predicates))
        rhs (propertyTranslation/translate (:object predicates))]
    (if (or (typeInference/is-object-property-expression? lhs)
            (typeInference/is-object-property-expression? rhs))
      (vector "EquivalentObjectProperties" lhs rhs)
      (vector "EquivalentProperties" lhs rhs))))

(defn translateType
  "Translate rdf:type for axioms"
  [predicateMap]
  (let [object (:object predicateMap)]
    (case object
      "owl:InverseFunctionalProperty" (translateInverseFunctionalProperty predicateMap)
      "owl:ReflexiveProperty" (translateReflexiveProperty predicateMap)
      "owl:IrreflexiveProperty" (translateIrreflexiveProperty predicateMap)
      "owl:AsymmetricProperty" (translateAsymmetricProperty predicateMap)
      "owl:SymmetricProperty" (translateSymmetricProperty predicateMap)
      "owl:TransitiveProperty" (translateTransitiveProperty predicateMap)
      "owl:FunctionalProperty" (translateFunctionalProperty predicateMap)
      "owl:AllDisjointProperties" (translateAllDisjointProperties predicateMap))))

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (let [p (:predicate predicateMap)];
    (case p
      "rdfs:subPropertyOf" (translateSubObjectPropertyOf predicateMap)
      "owl:propertyChainAxiom" (translatePropertyChainAxiom predicateMap)
      "owl:propertyDisjointWith" (translateDisjointProperties predicateMap)
      "rdfs:domain" (translateDomain predicateMap)
      "rdfs:range" (translateRange predicateMap)
      "owl:inverseOf" (translateInverseOf predicateMap)
      "rdf:type" (translateType predicateMap))))
