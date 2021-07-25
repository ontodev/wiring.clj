(ns wiring.thick2ofn.axiomTranslation.translate
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.expressionTranslation.propertyTranslation :as propertyTranslation]
            [wiring.thick2ofn.expressionTranslation.classTranslation :as CET]
            [wiring.thick2ofn.expressionTranslation.dataTypeTranslation :as DTT]
            [wiring.thick2ofn.axiomTranslation.objectPropertyAxioms :as OPA]
            [wiring.thick2ofn.axiomTranslation.classExpressionAxioms :as CEA]
            [wiring.thick2ofn.util :as util]
            [wiring.thick2ofn.spec :as owlspec]))

;TODO data validation
(declare translate)

(defn is-object-property-expression?
  "Checks whether an expression is a non-atomic OWL object property expression."
  [expression]
  (case (first expression)
      "ObjectInverseOf" true
      false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Ambiguous Property Axioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;NB this translation is used for both data properties and object properties
(defn translateFunctionalProperty
  "Translate owl:FunctionalProperty"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (if (is-object-property-expression? property)
      (vector "FunctionalObjectProperty" property)
      (vector "FunctionalProperty" property))))

(defn translateSubPropertyOf
  "Translate subPropertyOf"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        superProperty (propertyTranslation/translate (:object predicates))]
    (if (or (is-object-property-expression? subProperty)
            (is-object-property-expression? superProperty))
      (vector "ObjectSubPropertyOf" subProperty superProperty) 
      (vector "SubPropertyOf" subProperty superProperty))))

(defn translateDisjointProperties
  "Translate propertyDisjointWith"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        superProperty (propertyTranslation/translate (:object predicates))]
    (if (or (is-object-property-expression? subProperty)
            (is-object-property-expression? superProperty))
      (vector "ObjectDisjointProperties" subProperty superProperty)
      (vector "DisjointProperties" subProperty superProperty))))

(defn translateDomain
  "Translate rdfs:domain"
  [predicates]
  ;NB the domain is a class expression for both data and object properties  
  (let [property (propertyTranslation/translate (:subject predicates))
        domain (CET/translate (:object predicates))]
    (if (is-object-property-expression? property)
      (vector "PropertyDomain" property domain)
      (vector "ObjectPropertyDomain" property domain)))) 

(defn translateRange
  "Translate rdfs:domain"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))
        classRange (CET/translate (:object predicates))
        dataRange (DTT/translate (:object predicates))]
    (cond
     (CET/is-class-expression? classRange) (vector "ObjectPropertyDomain" property dataRange)
     (DTT/is-data-range-expression? dataRange) (vector "DataPropertyDomain" property dataRange)
     :else (vector "PropertyDomain"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn translateType
  "Translate rdf:type for axioms"
  [predicateMap]
  (let [object (:object predicateMap)]
    (if (string? object)
      "Thin triple"
      (case object
        ;ambiguous property axiom
        "owl:FunctionalProperty" (translateFunctionalProperty predicateMap)

        ;object property axiom
        "owl:InverseFunctionalProperty" (OPA/translate predicateMap)
        "owl:ReflexiveProperty" (OPA/translate predicateMap)
        "owl:IrreflexiveProperty" (OPA/translate predicateMap)
        "owl:AsymmetricProperty" (OPA/translate predicateMap)
        "owl:SymmetricProperty" (OPA/translate predicateMap)
        "owl:TransitiveProperty" (OPA/translate predicateMap)
        "";can't translate
        ))))

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

      ;ambiguous property axioms
      ;are translated as "object properties"
      ;because there is only one object property constructor that could infer the correct type

      ;that can be either object or data property axioms
      ;"rdfs:subPropertyOf" (translateSubPropertyOf predicateMap)
      ;"owl:propertyDisjointWith" (translateDisjointProperties predicateMap)
      ;"rdfs:domain" (translateDomain predicateMap)
      ;"rdfs:range" (translateRange predicateMap)

      "rdf:type" (translateType predicateMap);these are thin triples

      (str "Thin triple?" predicateMap)
      ;TODO: data property axioms
      ;TODO: data type definitions
      ;TODO: keys
      ;TODO: assertions
      ;TODO: annotations 
      )))
