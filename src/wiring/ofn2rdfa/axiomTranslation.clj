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

(defn translateList
  "Translate class expressions into an RDF list"
  [expressions subject2label htmlMarkup]
  (loop [in (rest (reverse expressions));constructing list from last element to first
         out (str "<span property=\"rdf:rest\" typeof=\"owl:Class\"> " 
                 htmlMarkup " " 
                 (classTranslation/translate (first (reverse expressions)) subject2label "rdf:first")
                  " <span resource=\"rdf:nil\" property=\"rdf:rest\"></span>"
                  "</span>")]
    (if (empty? in)
      out
      (recur (rest in)
             (if (empty? (rest in))
               (str (classTranslation/translate (first in) subject2label "rdf:first") " " out) 
               (str "<span property=\"rdf:rest\" typeof=\"owl:Class\"> "
                htmlMarkup " " (classTranslation/translate (first in) subject2label "rdf:first") " " out
               " </span>"))))))

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

(defn translateNaryDisjointClasses
  "Translate a DisjointClasses axiom"
  [ofn subject2label]
  ;note that this is not Manchester Syntax (because that is based on class frames rather than axioms)
  (let [[operator & arguments] ofn
      axiomOpening (str "DisjointClasses(<span typeof=\"owl:AllDisjointClasses\">")
      classOpening (str axiomOpening "<span typeof=\"owl:Class\" property=\"owl:members\">") 
      operands (str classOpening (translateList arguments subject2label ","))
      classClosing (str operands  " </span>")
      axiomClosing (str classClosing " </span>)")]
    axiomClosing))

;TODO test this
(defn translateBinaryDisjointClasses
  "Translate a DisjointClasses axiom"
  [ofn subject2label]
  (let [[op lhs rhs] ofn
        opening (spanOpening lhs)
        lhs (str opening " " (classTranslation/translate lhs subject2label))
        subclass (str lhs " DisjointWith ")
        rhs (str subclass (classTranslation/translate rhs subject2label "owl:disjointWith"))
        closing (str rhs " </span>")]
    closing)) 

(defn translateDisjointClasses
  "Translate a DisjointClasses axiom"
  [ofn subject2label]
  (if (= 3 (count ofn))
    (translateBinaryDisjointClasses ofn subject2label)
    (translateNaryDisjointClasses ofn subject2label)))

(defn translateDisjointUnion
  "Translate a DisjointUnion axiom"
  [ofn subject2label]
  (let [[operator lhs & arguments] ofn 
        opening (spanOpening lhs)
        lhs (str opening " " (classTranslation/translate lhs subject2label))
        disjointUnion (str lhs " DisjointUnionOf(")
        classOpening (str disjointUnion "<span typeof=\"owl:Class\" property=\"owl:disjointUnionOf\">") 
        operands (str classOpening (translateList arguments subject2label ","))
        classClosing (str operands " </span>")

        closing (str classClosing " </span>)")]
    closing))


(defn translateTwoEquivalentClasses
  "Translate a two equivalent classes"
  [classes subject2label]
  (let [[lhs rhs] classes
        opening (spanOpening lhs)
        lhs (str opening " " (classTranslation/translate lhs subject2label))
        subclass (str lhs " Equivalent to ")
        rhs (str subclass (classTranslation/translate rhs subject2label "owl:equivalentClass"))
        closing (str rhs " </span>")]
    closing))


;Note that this translates an EquivalentClasses axiom of the form
;[EquivalentClasses a b c d e]
;into 
;a owl:equivalentClass b
;b owl:equivalentClass c
;c owl:equivalentClass d
;d owl:equivalentClass e
(defn translateEquivalentClasses
  "Translate a DisjointClasses axiom"
  [ofn subject2label]
  (let [[operator & arguments] ofn
         pairs (keep #(if (= 2 (count %)) %) (partition-all 2 1 arguments))
         html (map #(translateTwoEquivalentClasses % subject2label) pairs)
         spaces (interpose, " <br> " html)
         string (apply str spaces)]
    string)) 

(defn translate
  "Translate OFN-S expression to tick triple"
  [ofn subject2label]
  (let [operator (first ofn)];
    ;(println predicateMap)
    (case operator
      ;class expression axioms
      "SubClassOf" (translateSubclassOf ofn subject2label)
      "DisjointUnion" (translateDisjointUnion ofn subject2label)
      "DisjointClasses" (translateDisjointClasses ofn subject2label) 
      "EquivalentClasses" (translateEquivalentClasses ofn subject2label) 
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
