(ns wiring.ofn2thick.axiomTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [wiring.ofn2thick.classTranslation :as classTranslation]
            [wiring.thick2ofn.spec :as spec])
  (:gen-class))

;TODO data validation
;TODO; make sure generated blank nodes are unique 
;(this is best done in a post-processing step I guess)
(declare translate)

(defn translateList
  "Translate class expressions into an RDF list"
  [expressions]
  (loop [in (reverse expressions);constructing list from last element to first
         out "\"rdf:nil\""]
    (if (empty? in)
      out
      (recur (rest in)
             (str "{\"rdf:first\": [{\"object\": " (classTranslation/translate (first in)) "}], " "\"rdf:rest\": [{\"object\": " out "}]}")))))

(defn translateEquivalentClasses
  "Translate a EquivalentClasses axiom"
  [ofn]
  (if (= 3 (count ofn))
    (let [[operator lhs rhs] ofn;non-list case
          subject (str "{\"subject\": " (classTranslation/translate lhs) ", ")
          predicate (str subject "\"predicate\": \"owl:equivalentClass\", ")
          object (str predicate "\"object\": " (classTranslation/translate rhs))
          closing (str object "}")]
      closing)
    (let [[operator & arguments] ofn;list case
          subject (str "{\"subject\": \"" (gensym "_:genid") "\", ");use class translation 
          predicate (str subject "\"predicate\": \"owl:equivalentClass\", ")
          operands (translateList arguments)
          object (str predicate "\"object\": " operands)
          closing (str object "}")]
      closing)))

(defn translateDisjointClasses
  "Translate a DisjointClasses axiom"
  [ofn]
  (let [[operator & arguments] ofn
        subject (str "{\"subject\": \"" (gensym "_:genid") "\", ");use class translation 
        predicate (str subject "\"predicate\": \"owl:allDisjointClasses\", ")
        operands (translateList arguments)
        object (str predicate "\"object\": {\"owl:members\": " operands "}")
        closing (str object "}")]
    closing))

(defn translateDisjointUnion
  "Translate a DisjointUnion axiom"
  [ofn]
  (let [[operator lhs & arguments] ofn
        subject (str "{\"subject\": " (classTranslation/translate lhs) ", ");use class translation 
        predicate (str subject "\"predicate\": \"owl:disjointUnionOf\", ")
        operands (translateList arguments)
        object (str predicate "\"object\": " operands)
        closing (str object "}")]
    closing))

(defn translateSubclassOf
  "Translate a SubClassOf axiom"
  [ofn]
  (let [[op lhs rhs] ofn
        subject (str "{\"subject\": " (classTranslation/translate lhs) ", ");use class translation
        predicate (str subject "\"predicate\": \"rdfs:subClassOf\", ")
        object (str predicate "\"object\": " (classTranslation/translate rhs))
        closing (str object "}")]
    closing))

(defn translate
  "Translate OFN-S expression to tick triple"
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
      (str \" ofn \"))))

