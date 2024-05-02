(ns wiring.ofn2rdfa.axiomTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [hiccup.core :as hicc]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2rdfa.classTranslation :as classTranslation]))

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

;HICCUP
(defn spanOpening
  "Determines the opening span for an RDFa serialistion."
  [input]
  (if (namedClass? input)
    {:about input}
    {:typeof (getType input)}))

;;HICCUP 
(defn translateList
  "Translate class expressions into an RDF list"
  [expressions subject2label htmlMarkup]
  (loop [in (rest (reverse expressions));constructing list from last element to first
         out [:span {:property "rdf:rest", :typeof "rdf:List"}
              htmlMarkup
              " "
              (classTranslation/translate (first (reverse expressions)) subject2label "rdf:first")
              [:span {:resource "rdf:nil", :property "rdf:rest"}]]]
    (if (empty? in)
      out
      (recur (rest in)
             (if (empty? (rest in))
               ;(conj out (classTranslation/translate (first in) subject2label "rdf:first"))
               [:span (classTranslation/translate (first in) subject2label "rdf:first") out]
               [:span {:property "rdf:rest", :typeof "rdf:List"}
                htmlMarkup
                " "
                (classTranslation/translate (first in) subject2label "rdf:first")
                out])))))

(defn translateSubclassOf
  "Translate a SubClassOf axiom"
  [ofn subject2label]
  (let [[op lhs rhs] ofn
        opening (spanOpening lhs)
        subclass (classTranslation/translate lhs subject2label)
        superclass (classTranslation/translate rhs subject2label "rdfs:subClassOf")]
    [:span opening subclass " SubClassOf " superclass]))

(defn translateNaryDisjointClasses
  "Translate a DisjointClasses axiom"
  [ofn subject2label]
  ;note that this is not Manchester Syntax (because that is based on class frames rather than axioms)
  (let [[operator & arguments] ofn
        operands (translateList arguments subject2label ",")]
    [:div "DisjointClasses("
     [:span {:typeof "owl:AllDisjointClasses"}
      [:span {:typeof "rdf:List", :property "owl:members"}
       operands]]
     ")"]))

(defn translateBinaryDisjointClasses
  "Translate a DisjointClasses axiom"
  [ofn subject2label]
  (let [[op lhs rhs] ofn
        opening (spanOpening lhs)
        lhs (classTranslation/translate lhs subject2label)
        rhs (classTranslation/translate rhs subject2label "owl:disjointWith")]
    [:span opening lhs " DisjointWith " rhs]))

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
        lhs (classTranslation/translate lhs subject2label)
        operands (translateList arguments subject2label ",")]
    [:span opening lhs " DisjointUnionOf("  [:span {:typeof "rdf:List", :property "owl:disjointUnionOf"} operands ")"]]))

(defn translateTwoEquivalentClasses
  "Translate a two equivalent classes"
  [classes subject2label]
  (let [[lhs rhs] classes
        opening (spanOpening lhs)
        left (classTranslation/translate lhs subject2label)
        right (classTranslation/translate rhs subject2label "owl:equivalentClass")]
    [:span opening left " Equivalent to " right]))

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
        spaces (interpose, [:br] html)]
    spaces))

(defn translate
  "Translate OFN-S expression to tick triple"
  [ofn subject2label]
  (let [operator (first ofn)];
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
