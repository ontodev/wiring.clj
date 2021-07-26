(ns wiring.thick2ofn.axiomTranslation.translate
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.expressionTranslation.propertyTranslation :as propertyTranslation] 
            [wiring.thick2ofn.typeInference :as typeInference]
            [wiring.thick2ofn.expressionTranslation.classTranslation :as CET]
            [wiring.thick2ofn.expressionTranslation.dataTypeTranslation :as DTT]
            [wiring.thick2ofn.axiomTranslation.objectPropertyAxioms :as OPA]
            [wiring.thick2ofn.axiomTranslation.classExpressionAxioms :as CEA]
            [wiring.thick2ofn.util :as util]
            [wiring.thick2ofn.spec :as owlspec]))

;TODO data validation
(declare translate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Ambiguous Property Axioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;NB this translation is used for both data properties and object properties
(defn translateFunctionalProperty
  "Translate owl:FunctionalProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (if (typeInference/is-object-property-expression? property)
      (vector "FunctionalObjectProperty" property)
      (vector "FunctionalProperty" property))))

(defn translateSubPropertyOf
  "Translate subPropertyOf"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        superProperty (propertyTranslation/translate (:object predicates))]
    (if (or (typeInference/is-object-property-expression? subProperty)
            (typeInference/is-object-property-expression? superProperty))
      (vector "ObjectSubPropertyOf" subProperty superProperty) 
      (vector "SubPropertyOf" subProperty superProperty))))

(defn translateDisjointProperties
  "Translate propertyDisjointWith"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        superProperty (propertyTranslation/translate (:object predicates))]
    (if (or (typeInference/is-object-property-expression? subProperty)
            (typeInference/is-object-property-expression? superProperty))
      (vector "ObjectDisjointProperties" subProperty superProperty)
      (vector "DisjointProperties" subProperty superProperty))))

(defn translateDomain
  "Translate rdfs:domain"
  [predicates]
  ;NB the domain is a class expression for both data and object properties  
  (let [property (propertyTranslation/translate (:subject predicates))
        domain (CET/translate (:object predicates))]
    (if (typeInference/is-object-property-expression? property)
      (vector "PropertyDomain" property domain)
      (vector "ObjectPropertyDomain" property domain)))) 

(defn translateRange
  "Translate rdfs:domain"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))
        classRange (CET/translate (:object predicates));tentative translation
        dataRange (DTT/translate (:object predicates))];tentative translation
    (cond
     (typeInference/is-class-expression? classRange) (vector "ObjectPropertyDomain" property classRange)
     (typeInference/is-data-range-expression? dataRange) (vector "DataPropertyDomain" property dataRange)
     (typeInference/is-ambiguous-expression? classRange) (vector "PropertyDomain" property classRange)
     (typeInference/is-ambiguous-expression? dataRange) (vector "PropertyDomain" property dataRange)
     :else (vector "PropertyDomain" property (:object predicates)))))

(defn translateAllDisjointProperties 
  "Translate owl:AllDisjointProperties"
  [predicates]
  (let [arguments (propertyTranslation/translateList (:owl:members (:object predicates)))]
    (if (some typeInference/is-object-property-expression? arguments)
      (vec (cons "DisjointObjectProperties" arguments))
      (vec (cons "DisjointProperties" arguments)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn translateType
  "Translate rdf:type for axioms"
  [predicateMap]
  (let [object (:object predicateMap)]
    (case object
      ;ambiguous property axiom
      "owl:FunctionalProperty" (translateFunctionalProperty predicateMap)
      "owl:AllDisjointProperties" (translateAllDisjointProperties predicateMap)

      ;object property axiom
      "owl:InverseFunctionalProperty" (OPA/translate predicateMap)
      "owl:ReflexiveProperty" (OPA/translate predicateMap)
      "owl:IrreflexiveProperty" (OPA/translate predicateMap)
      "owl:AsymmetricProperty" (OPA/translate predicateMap)
      "owl:SymmetricProperty" (OPA/translate predicateMap)
      "owl:TransitiveProperty" (OPA/translate predicateMap)
      "";can't translate
      )))

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (let [p (:predicate predicateMap)];
    (case p
      ;class expression axioms
      "rdfs:subClassOf" (CEA/translateSubclassOf predicateMap)
      "owl:disjointUnionOf" (CEA/translateDisjointUnionOf predicateMap)
      "owl:equivalentClass" (CEA/translateEquivalentClasses predicateMap)
      "owl:AllDisjointClasses" (CEA/translateAllDisjointClasses predicateMap)
      "owl:disjointWith" (CEA/translateDisjointWith predicateMap) 

      ;object property  axioms
      "owl:inverseOf" (OPA/translateInverseOf predicateMap)
      "owl:propertyChainAxiom" (OPA/translateSubObjectPropertyOf predicateMap)
      "owl:InverseFunctionalProperty" (OPA/translateInverseFunctionalProperty predicateMap)
      "owl:ReflexiveProperty" (OPA/translateReflexiveProperty predicateMap) 
      "owl:IrreflexiveProperty" (OPA/translateIrreflexiveProperty predicateMap) 
      "owl:AsymmetricProperty" (OPA/translateAsymmetricProperty predicateMap) 
      "owl:SymmetricProperty" (OPA/translateSymmetricProperty predicateMap) 
      "owl:TransitiveProperty" (OPA/translateTransitiveProperty predicateMap)

      ;ambiguous property axioms (for data and object property axioms)
      ;NB: all data property axioms are ambiguous
      "rdfs:subPropertyOf" (translateSubPropertyOf predicateMap)
      "owl:propertyDisjointWith" (translateDisjointProperties predicateMap)
      "rdfs:domain" (translateDomain predicateMap)
      "rdfs:range" (translateRange predicateMap)

      "rdf:type" (translateType predicateMap)

      (str "Thin triple?" predicateMap)
      ;TODO: data type definitions
      ;TODO: keys
      ;TODO: assertions
      ;TODO: annotations 
      )))
