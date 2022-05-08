(ns wiring.ofn2ldtab.axiomTranslation.classExpressionAxiom
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [wiring.ofn2ldtab.util :as u]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2ldtab.expressionTranslation.classTranslation :as classTranslation]
            [wiring.ofn2ldtab.spec :as owlspec])
  (:gen-class))

;TODO; make sure generated blank nodes are unique 
;(this is best done in a post-processing step I guess)
(declare translate)

(defn translateList
  "Translate class expressions into an RDF list"
  [expressions]
  (loop [in (reverse expressions);constructing list from last element to first
         out "rdf:nil"]
    (if (empty? in)
      out
      (recur (rest in)
             {:rdf:first [{:object (classTranslation/translate (first in)) }]
              :rdf:rest [{:object out}]}))))

(defn translateEquivalentClasses
  "Translate a EquivalentClasses axiom"
  [ofn]
  {:pre [(spec/valid? ::owlspec/equivalentClasses ofn)]}
  (if (= 3 (count ofn))
    (let [[operator lhs rhs] ofn;non-list case
          triple {:subject (classTranslation/translate lhs)
                  :predicate "owl:equivalentClass"
                  :object (classTranslation/translate rhs)}]
      triple) 
    (let [[operator & arguments] ofn;list case
          triple {:subject (gensym "_:genid")
                  :predicate "owl:equivalentClass"
                  :object (translateList arguments)}]
      triple))) 

(defn translateDisjointClasses
  "Translate a DisjointClasses axiom"
  [ofn]
  {:pre [(spec/valid? ::owlspec/disjointClasses ofn)]}
  (let [[operator & arguments] ofn
        triple {:subject (gensym "_:genid")
                :predicate "owl:AllDisjointClasses"
                :object {:owl:members (translateList arguments)}}
        tuple {:subject (classTranslation/translate (first arguments))
               :predicate "owl:disjointWith"
               :object (classTranslation/translate (second arguments))}]
    (if (= (count arguments) 2)
      tuple
      triple)))

(defn translateDisjointUnion
  "Translate a DisjointUnion axiom"
  [ofn]
  {:pre [(spec/valid? ::owlspec/disjointUnion ofn)]}
  (let [[operator lhs & arguments] ofn
        triple {:subject (classTranslation/translate lhs)
                :predicate "owl:disjointUnionOf"
                :object (translateList arguments)}]
    triple)) 

(defn translateSubclassOf
  "Translate a SubClassOf axiom"
  [ofn]
  {:pre [(spec/valid? ::owlspec/subclassOf ofn)]}
  (let [[op lhs rhs] ofn
        triple {:subject (classTranslation/translate lhs)
                :predicate "rdfs:subClassOf"
                :object (classTranslation/translate rhs)
                :datatype (u/translate-datatype rhs)
                ;TODO check whether object is literal/URI/CURIE
                
                }]
    triple))

(defn translateThinTriple
  "Translate Thin Triples"
  [ofn]
  (let [[op s p o] ofn
        triple {:subject s
                :predicate p
                :object o}]
    triple)) 

(defn translate
  "Translate OFN-S expression to thick triple"
  [ofn]
  (let [operator (first ofn)];
    (case operator
      ;class expression axioms
      "SubClassOf" (translateSubclassOf ofn)
      "DisjointUnion" (translateDisjointUnion ofn)
      "DisjointClasses" (translateDisjointClasses ofn)
      "EquivalentClasses" (translateEquivalentClasses ofn)
      ;"ThinTriple" (translateThinTriple ofn) ; this case should not be called
      (str "ERROR: " ofn)))) ;this case should also not be called

