(ns wiring.ofn2rdfa.axiomTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2rdfa.classTranslation :as classTranslation]))
            ;[wiring.thick2man.util :as util]
            ;[wiring.thick2man.spec :as owlspec]))

;TODO data validation
(declare translate)

;determine type first
(defn namedClass?
  "Determines the rdf:type of a class expression"
  [ofn]
  (let [operator (first ofn)]
    (case operator 
      "ObjectSomeValuesFrom" false
      "ObjectAllValuesFrom" false
      "ObjectHasValue" false
      "ObjectMinCardinality" false
      "ObjectMaxCardinality" false
      "ObjectExactCardinality" false
      "ObjectHasSelf" false
      "ObjectIntersectionOf" false
      "ObjectUnionOf" false
      "ObjectOneOf" false
      "ObjectComplementOf" false
      true)))

;determine type first
(defn getType
  "Determines the rdf:type of a class expression"
  [ofn]
  (let [operator (first ofn)]
    (case operator 
      "ObjectSomeValuesFrom" "owl:Restriction"
      "ObjectAllValuesFrom" "owl:Restriction"
      "ObjectHasValue" "owl:Restriction"
      "ObjectMinCardinality" "owl:Restriction"
      "ObjectMaxCardinality" "owl:Restriction"
      "ObjectExactCardinality" "owl:Restriction"
      "ObjectHasSelf" "owl:Restriction"
      "ObjectIntersectionOf" "owl:Class"
      "ObjectUnionOf" "owl:Class"
      "ObjectOneOf" "owl:Class"
      "ObjectComplementOf" "owl:Class"
      ofn)));return input as named class

(defn spanOpening
  "Determines the opening span for an RDFa serialistion."  
  [input]
(if (namedClass? input)
  (str "<span about=" \" input \" ">")
  (str "<span typeof=" \"  (getType input) \" ">")))

(defn translateSubclassOf
  "Translate a SubClassOf axiom"
  [ofn subject2label]
  (let [[op lhs rhs] ofn
        opening (spanOpening lhs)
        lhs (str opening " " (classTranslation/translate lhs subject2label))
        subclass (str lhs " SubClassOf ")
        rhs (str subclass (classTranslation/translate rhs subject2label "rdfs:subClassOf"))
        closing (str rhs " </span>")]
    closing))

(defn translate
  "Translate OFN-S expression to tick triple"
  [ofn subject2label]
  (let [operator (first ofn)];
    ;(println predicateMap)
    (case operator
      ;class expression axioms
      "SubClassOf" (translateSubclassOf ofn subject2label)
      ;"DisjointUnion" (translateDisjointUnion ofn)
      ;"DisjointClasses" (translateDisjointClasses ofn)
      ;"EquivalentClasses" (translateEquivalentClasses ofn)
      ;;object property  axioms
      ;"rdfs:subPropertyOf" (translateSubObjectPropertyOf predicateMap)
      ;"owl:propertyChainAxiom" (translateSubObjectPropertyOf predicateMap)
      ;"owl:propertyDisjointWith" (translateDisjointProperties predicateMap)
      ;"rdfs:domain" (translateDomain predicateMap)
      ;"rdfs:range" (translateRange predicateMap)
      ;"owl:inverseOf" (translateInverseOf predicateMap)
      ;"rdf:type" (translateType predicateMap)
      ;"Thin triple?"
      ;TODO: data property axioms
      ;TODO: data type definitions
      ;TODO: keys
      ;TODO: assertions
      ;TODO: annotations 
      (str \" ofn \"))))
