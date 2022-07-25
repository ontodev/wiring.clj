(ns wiring.ofn2ldtab.axiomTranslation.classExpressionAxiom
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [wiring.ofn2ldtab.util :as u]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2ldtab.expressionTranslation.classTranslation :as classTranslation]
            [wiring.ofn2ldtab.annotationTranslation.translate :as ann]
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
  [ofn graph]
  ;{:pre [(spec/valid? ::owlspec/equivalentClasses ofn)]}
  (let [annotation (ann/get-annotation ofn)
        ofn (ann/get-owl ofn)]

  (if (= 3 (count ofn)) 
    (let [[_operator lhs rhs] ofn;non-list case
          object (classTranslation/translate rhs)
          triple {:assertion 1
                  :retraction 0
                  :graph graph
                  :subject (classTranslation/translate lhs)
                  :predicate "owl:equivalentClass"
                  :object object
                  :datatype (u/translate-datatype object)
                  :annotation (ann/translate annotation) }]
      triple) 
    (let [[_operator & arguments] ofn;list case
          object (translateList arguments)
          triple {:assertion 1
                  :retraction 0
                  :graph graph
                  :subject (gensym "_:genid")
                  :predicate "owl:equivalentClass"
                  :object object
                  :datatype "_JSON"
                  :annotation (ann/translate annotation) }]
      triple))))

(defn translateDisjointClasses
  "Translate a DisjointClasses axiom"
  [ofn graph]
  ;{:pre [(spec/valid? ::owlspec/disjointClasses ofn)]}
  (let [annotation (ann/get-annotation ofn)
        ofn (ann/get-owl ofn) 
        [_operator & arguments] ofn
        triple {:assertion 1
                :retraction 0
                :graph graph
                :subject (gensym "_:genid")
                :predicate "owl:AllDisjointClasses"
                :object {:owl:members (translateList arguments)}
                :datatype "_JSON"
                :annotation (ann/translate annotation) } 
        object (classTranslation/translate (second arguments))
        tuple {:assertion 1
               :retraction 0
               :graph graph
               :subject (classTranslation/translate (first arguments))
               :predicate "owl:disjointWith"
               :object object
               :datatype (u/translate-datatype object)
               :annotation (ann/translate annotation) }]
    (if (= (count arguments) 2)
      tuple
      triple)))

(defn translateDisjointUnion
  "Translate a DisjointUnion axiom"
  [ofn graph]
  ;{:pre [(spec/valid? ::owlspec/disjointUnion ofn)]}
  (let [annotation (ann/get-annotation ofn)
        ofn (ann/get-owl ofn)
        [_operator lhs & arguments] ofn
        object (translateList arguments)
        triple {:assertion 1;TODO
                :retraction 0
                :graph graph
                :subject (classTranslation/translate lhs)
                :predicate "owl:disjointUnionOf"
                :object object
                :datatype (u/translate-datatype object)
                :annotation (ann/translate annotation) }]
    triple)) 

(defn translateSubclassOf
  "Translate a SubClassOf axiom"
  [ofn graph]
  ;{:pre [(spec/valid? ::owlspec/subclassOf ofn)]}
  (let [annotation (ann/get-annotation ofn)
        [_op lhs rhs]  (ann/get-owl ofn)
        triple {:assertion 1 ;TODO 
                :retraction 0
                :graph graph
                :subject (classTranslation/translate lhs)
                :predicate "rdfs:subClassOf"
                :object (classTranslation/translate rhs)
                :datatype (u/translate-datatype rhs)
                :annotation (ann/translate annotation) }]
    triple))

(defn translateThinTriple
  "Translate Thin Triples"
  [ofn graph]
  (let [annotation (ann/get-annotation ofn)
        ofn (ann/get-owl ofn)
        [_op s p o] ofn
        triple {:assertion 1 ;TODO
                :retraction 0
                :graph graph
                :subject s
                :predicate p
                :object o
                :datatype (u/translate-datatype o)
                :annotation (ann/translate annotation) }]
    triple)) 

(defn translate
  "Translate OFN-S expression to thick triple"
  [ofn graph]
  (let [operator (first ofn)];
    (case operator
      ;class expression axioms
      "SubClassOf" (translateSubclassOf ofn graph)
      "DisjointUnion" (translateDisjointUnion ofn graph)
      "DisjointClasses" (translateDisjointClasses ofn graph)
      "EquivalentClasses" (translateEquivalentClasses ofn graph)
      "ThinTriple" (translateThinTriple ofn graph) ;TODO: do we need "untyped" ThinTriples?
      (str "ERROR: " ofn)))) ;this case should also not be called

