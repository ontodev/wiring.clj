(ns wiring.ofn2man.axiomTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            ;[wiring.thick2man.propertyTranslation :as propertyTranslation]
            [wiring.ofn2man.classTranslation :as classTranslation]))
            ;[wiring.thick2man.util :as util]
            ;[wiring.thick2man.spec :as owlspec]))

;TODO data validation
(declare translate)

(defn namedClass?
  "Checks whether an expression is a named class."
  [ofn]
  (let [operator (first ofn)];
    (case operator
      "ObjectSomeValuesFrom" false
      "ObjectAllValuesFrom" false
      "ObjectHasValue"  false
      "ObjectMinCardinality" false
      "ObjectMaxCardinality" false
      "ObjectExactCardinality" false
      "ObjectIntersectionOf" false
      "ObjectUnionOf" false
      "ObjectOneOf" false
      "ObjectComplementOf" false
      true)))

;NOTE this only translate a single axiom
;however, manchester syntax operates on class frames
;so, this function cannot be used to construct an ontology in manchester syntax
(defn translateSubclassOf
  "Translate a SubClassOf axiom"
  [ofn]
  (let [[op lhs rhs] ofn
        subject (str "Class: " (classTranslation/translate lhs) "\n\n");LHS is required to be a named class
        predicate (str subject (apply str (repeat 4 " ")) "SubClassOf:\n")
        object (str predicate (apply str (repeat 8 " ")) (classTranslation/translate rhs))]
    object))

(defn translateDisjointClasses
  "Translate a DisjointClasses axiom"
  [ofn]
  (let [[operator & arguments] ofn
        disjoint "DisjointClasses:\n"
        indent (str disjoint (apply str (repeat 4 " ")))
        arguments (map #(if (namedClass? %) % (str "("  (classTranslation/translate %) ")")) arguments)
        args (str indent (apply str (interpose "," arguments)))]
    args))

(defn translateDisjointUnion
  "Translate a DisjointUnion axiom"
  [ofn]
  (let [[operator lhs & arguments] ofn
        clas (str "Class: "  lhs "\n")
        indent1 (str clas (apply str (repeat 4 " ")))
        disjointUnion (str indent1 "DisjointUnionOf:\n")
        indent2 (str disjointUnion (apply str (repeat 8 " ")))
        arguments (map #(if (namedClass? %) % (str "("  (classTranslation/translate %) ")")) arguments)
        args (str indent2 (apply str (interpose ", " arguments)))]
    args))

;NB manchester syntax would require the enumeration of all possible class frames
;but again - this is only an axiom based translation
(defn translateEquivalentClasses
  "Translate a EquivalentClasses axiom"
  [ofn]
  (let [[operator lhs & arguments] ofn
        clas (str "Class: "  lhs "\n")
        indent1 (str clas (apply str (repeat 4 " ")))
        equivalence (str indent1 "EquivalentTo:\n")
        arguments (map #(if (namedClass? %) % (str "("  (classTranslation/translate %) ")")) arguments)
        indentArgs (map #(str (apply str (repeat 8 " ")) %) arguments)
        args (str equivalence (apply str (interpose ",\n" indentArgs)))]
    args))

;NOTE: this translate a single axiom into manchester syntax 
;however, manchester sytnax is based on class frames.
(defn translate
  "Translate predicate map to OFS."
  [ofn]
  (let [operator (first ofn)];
    ;(println predicateMap)
    (case operator
      ;class expression axioms
      "SubClassOf" (translateSubclassOf ofn)
      "DisjointUnion" (translateDisjointUnion ofn)
      "DisjointClasses" (translateDisjointClasses ofn)
      "EquivalentClasses" (translateEquivalentClasses ofn)
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
      ofn)))
