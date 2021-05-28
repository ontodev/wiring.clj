(ns wiring.thick2ofn.axiomTranslation 
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.propertyTranslation :as propertyTranslation]
            [wiring.thick2ofn.classTranslation :as classTranslation]
            [wiring.thick2ofn.util :as util]
            [wiring.thick2ofn.spec :as owlspec]))

;TODO data validation
(declare translate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Class Expression Axioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateSubclassOf
  "Translate a SubClassOf axiom"
  [predicates]
  (let [subclass (classTranslation/translate (:subject predicates))
        superclass (classTranslation/translate (:object predicates))]
    (util/ofsFormat "SubClassOf" subclass superclass)))

(defn translateDisjointUnionOf
  "Translate a DisjointUnion axiom"
  [predicates]
  (let [subject (classTranslation/translate (:subject predicates))
        arguments (classTranslation/translate (:object predicates))]
    (util/ofsFormat "DisjointUnion" subject arguments)))

(defn translateAllDisjointClasses
  "Translate DisjointClasses axiom"
  [predicates]
  (let [arguments (classTranslation/translate (:owl:members (:object predicates)))]
  ;(let [arguments (classTranslation/translate (:object predicates))]
    (util/ofsFormat "DisjointClasses" arguments)))

(defn translateEquivalentClasses
  "Translate Equivalent Class axiom"
  [predicates]
  (let [subject (classTranslation/translate (:subject predicates))
        arguments (classTranslation/translate (:object predicates))
        nAry (contains? (:object predicates) :rdf:first)]
    (if nAry
      (util/ofsFormat "EquivalentClasses" arguments);EquivalentClasses(e1, ..., en)
      (util/ofsFormat "EquivalentClasses" subject arguments))));EquivalentClasses(e1,e2) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Object Property Axioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(defn translateThinPropertyAxiom
  "Translate thin property axioms"
  [predicates description]
  (let [property (propertyTranslation/translate (:subject predicates))]
    (util/ofsFormat description property)))

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
        "owl:FunctionalProperty" (translateFunctionalProperty predicateMap)
        "owl:inverseFunctionalProperty" (translateInverseFunctionalProperty predicateMap)
        "owl:ReflexiveProperty" (translateThinPropertyAxiom predicateMap "ReflexiveObjectProperty")
        "";can't translate
        ))))

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (let [p (:predicate predicateMap)];
    ;(println predicateMap)
    ;(println p) 
    (case p
      ;class expression axioms
      "rdfs:subClassOf" (translateSubclassOf predicateMap)
      "owl:disjointUnionOf" (translateDisjointUnionOf predicateMap)
      "owl:equivalentClass" (translateEquivalentClasses predicateMap)
      "owl:AllDisjointClasses" (translateAllDisjointClasses predicateMap)
      ;object property  axioms
      "rdfs:subPropertyOf" (translateSubObjectPropertyOf predicateMap)
      "owl:propertyChainAxiom" (translateSubObjectPropertyOf predicateMap)
      "owl:propertyDisjointWith" (translateDisjointProperties predicateMap)
      "rdfs:domain" (translateDomain predicateMap)
      "rdfs:range" (translateRange predicateMap)
      "owl:inverseOf" (translateInverseOf predicateMap)
      "rdf:type" (translateType predicateMap)
      "Thin triple?"
      ;TODO: data property axioms
      ;TODO: data type definitions
      ;TODO: keys
      ;TODO: assertions
      ;TODO: annotations 
      )))
