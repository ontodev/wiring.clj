(ns wiring.thick2ofn.axiomTranslation.translate
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.propertyTranslation :as propertyTranslation] ;TODO change this
            [wiring.thick2ofn.classTranslation :as classTranslation];TODO
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
    (util/ofsFormat "FunctionalProperty" property)))

(defn translateSubPropertyOf
  "Translate subPropertyOf"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        superProperty (propertyTranslation/translate (:object predicates))]
    (util/ofsFormat "SubPropertyOf" subProperty superProperty)))

(defn translateDisjointProperties
  "Translate propertyDisjointWith"
  [predicates]
  (let [subProperty (propertyTranslation/translate (:subject predicates))
        superProperty (propertyTranslation/translate (:object predicates))]
    (util/ofsFormat "DisjointProperties" subProperty superProperty)))

(defn translateDomain
  "Translate rdfs:domain"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))
        domain (classTranslation/translate (:object predicates))];cannot translate this then,..
    (util/ofsFormat "PropertyDomain" property domain)))
;TODO: in case of ambiguity: try both translations - if one is sucessful winner -
;if both are successsful, and it's not a named entity, then it's ambiguous all the way trhough


(defn translateRange
  "Translate rdfs:domain"
  [predicates]
  (let [property (propertyTranslation/translate (:subject predicates))
        rang (classTranslation/translate (:object predicates))];cannot translate this then,..
    (util/ofsFormat "PropertyDomain" property rang))) 


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
