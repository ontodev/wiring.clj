(ns wiring.ldtab2ofn.axiomTranslation.translate
  (:require [clojure.repl :as repl]
            [clojure.string :as str]
            [clojure.spec.alpha :as spec]
            [wiring.ldtab2ofn.expressionTranslation.propertyTranslation :as propertyTranslation] 
            [wiring.ldtab2ofn.typeInference :as typeInference]
            [wiring.ldtab2ofn.expressionTranslation.classTranslation :as CET]
            [wiring.ldtab2ofn.expressionTranslation.dataTypeTranslation :as DTT]
            [wiring.ldtab2ofn.axiomTranslation.objectPropertyAxioms :as OPA]
            [wiring.ldtab2ofn.axiomTranslation.classExpressionAxioms :as CEA]
            [wiring.ldtab2ofn.util :as util]
            [wiring.ldtab2ofn.spec :as owlspec]))

;TODO data validation
(declare translate)
;TODO handle datatype key
;TODO handle annotations 

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
     (typeInference/is-class-expression? classRange) (vector "ObjectPropertyRange" property classRange)
     (typeInference/is-data-range-expression? dataRange) (vector "DataPropertyRange" property dataRange)
     (typeInference/is-ambiguous-expression? classRange) (vector "PropertyRange" property classRange)
     (typeInference/is-ambiguous-expression? dataRange) (vector "PropertyRange" property dataRange)
     :else (vector "PropertyRange" property (:object predicates)))))

(defn translateAllDisjointProperties 
  "Translate owl:AllDisjointProperties"
  [predicates]
  (let [arguments (propertyTranslation/translateList (:owl:members (:object predicates)))]
    (if (some typeInference/is-object-property-expression? arguments)
      (vec (cons "DisjointObjectProperties" arguments))
      (vec (cons "DisjointProperties" arguments)))))

(defn translateAllDifferent
  [predicates]
  (vec (cons "DifferentIndividuals" (CET/translateList (:object (first (:owl:distinctMembers (:object predicates)))))))) ;TODO this is not a class translation



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn translateThinTriples
  "Translate ThinTriples"
  [predicateMap]
  (let [subject (:subject predicateMap)
        predicate (:predicate predicateMap)
        object (:object predicateMap)
        datatype (:datatype predicateMap)]
    (if (and (string? subject)
             (string? predicate)
             (string? object))
      (vector "ThinTriple" subject predicate (util/encode-entity object datatype))
      (vector "Error" subject predicate object))))

(defn translateIndividualAssertion
  "Translate class assertion with complex class"
  [predicateMap]
  (let [subject (:subject predicateMap)
        predicate (:predicate predicateMap)
        object (CET/translate (:object predicateMap))]
    (vector "ClassAssertion" subject object)))

(defn translateSpecialCases 
  [predicateMap]
  (let [object (:object predicateMap)]
    (if (map? object)
      (translateIndividualAssertion predicateMap)
      (translateThinTriples predicateMap)))) 

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
      (translateSpecialCases predicateMap)
      ;(map? ob) (translateIndividualAssertion predicateMap) 
      ;(translateThinTriples predicateMap)
      )))

;TODO annotations
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
      "owl:InverseFunctionalProperty" (OPA/translateInverseFunctionalProperty predicateMap)
      "owl:ReflexiveProperty" (OPA/translateReflexiveProperty predicateMap) 
      "owl:IrreflexiveProperty" (OPA/translateIrreflexiveProperty predicateMap) 
      "owl:AsymmetricProperty" (OPA/translateAsymmetricProperty predicateMap) 
      "owl:SymmetricProperty" (OPA/translateSymmetricProperty predicateMap) 
      "owl:TransitiveProperty" (OPA/translateTransitiveProperty predicateMap)
      "owl:propertyChainAxiom" (OPA/translatePropertyChainAxiom predicateMap)
      "owl:equivalentProperty" (OPA/translateEquivalentProperty predicateMap) ;TODO this currently only translates to OFN S-expressions with 2 arguments - but this is an n-ary constructor

      ;ambiguous property axioms (for data and object property axioms)
      ;NB: all data property axioms are ambiguous
      "rdfs:subPropertyOf" (translateSubPropertyOf predicateMap)
      "owl:propertyDisjointWith" (translateDisjointProperties predicateMap)
      "rdfs:domain" (translateDomain predicateMap)
      "rdfs:range" (translateRange predicateMap)
      "owl:AllDifferent" (translateAllDifferent predicateMap)

      ;TODO: individual assertions
      "rdf:type" (translateType predicateMap)

      (translateThinTriples predicateMap)

      ;TODO: data type definitions
      ;TODO: keys
      ;TODO: assertions
      ;TODO: annotations 
      )))
