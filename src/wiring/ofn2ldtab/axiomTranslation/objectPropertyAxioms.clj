(ns wiring.ofn2ldtab.axiomTranslation.objectPropertyAxioms
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2ldtab.expressionTranslation.propertyTranslation :as propertyTranslation]
            [wiring.ofn2ldtab.expressionTranslation.classTranslation :as classTranslation]
            [wiring.ofn2ldtab.annotationTranslation.translate :as ann]
            [wiring.ofn2ldtab.spec :as owlspec])
  (:gen-class))


;TODO: complete LDTab thick triples (graph, assertion, etc.)


(defn translateList
  "Translate class expressions into an RDF list"
  [expressions]
  (loop [in (reverse expressions);constructing list from last element to first
         out "rdf:nil"]
    (if (empty? in)
      out
      (recur (rest in)
             {:rdf:first [{:object (propertyTranslation/translate (first in))}]
              :rdf:rest [{:object out}]}))))

(defn translateSubObjectPropertyOf
  "Translate SubObjectPropertyOf"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op sub sup] owl]
    (if (coll? sub)
      {:subject (propertyTranslation/translate sup)
       :predicate "owl:propertyChainAxiom"
       :object (propertyTranslation/translate sub)
       :annotation (ann/translate annotation)}
      {:subject (propertyTranslation/translate sub)
       :predicate "rdfs:subPropertyOf"
       :object (propertyTranslation/translate sup)
       :annotation (ann/translate annotation)})))

(defn translateDomain
  "Translate property domain"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op s o] owl
        triple {:subject (propertyTranslation/translate s)
                :predicate "rdfs:domain"
                :object (classTranslation/translate o)
                :annotation (ann/translate annotation)}]
    triple))

(defn translateRange
  "Translate property range"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op s o] owl
        triple {:subject (propertyTranslation/translate s)
                :predicate "rdfs:range"
                :object (classTranslation/translate o)
                :annotation (ann/translate annotation)}]
    triple))

(defn translateInverseOf
  "Translate inverse of"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op s o] owl
        triple {:subject (propertyTranslation/translate s)
                :predicate "owl:inverseOf"
                :object (propertyTranslation/translate o)
                :annotation (ann/translate annotation)}]
    triple))

(defn translateFunctionalProperty
  "Translate FunctionalProperty"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op s] owl
        triple {:subject (propertyTranslation/translate s)
                :predicate "rdf:type"
                :object "owl:FunctionalProperty"
                :annotation (ann/translate annotation)}]
    triple))

(defn translateInverseFunctionalProperty
  "Translate InverseFunctionalProperty"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op s] owl
        triple {:subject (propertyTranslation/translate s)
                :predicate "rdf:type"
                :object "owl:InverseFunctionalProperty"
                :annotation (ann/translate annotation)}]
    triple))

(defn translateReflexiveProperty
  "Translate ReflexiveProperty"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op s] owl
        triple {:subject (propertyTranslation/translate s)
                :predicate "rdf:type"
                :object "owl:ReflexiveProperty"
                :annotation (ann/translate annotation)}]
    triple))

(defn translateIrreflexiveProperty
  "Translate IrreflexiveObjectProperty"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op s] owl
        triple {:subject (propertyTranslation/translate s)
                :predicate "rdf:type"
                :object "owl:IrreflexiveProperty"
                :annotation (ann/translate annotation)}]
    triple))

(defn translateAsymmetricProperty
  "Translate AsymmetricObjectProperty"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op s] owl
        triple {:subject (propertyTranslation/translate s)
                :predicate "rdf:type"
                :object "owl:AsymmetricProperty"
                :annotation (ann/translate annotation)}]
    triple))

(defn translateSymmetricProperty
  "Translate SymmetricObjectProperty"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op s] owl
        triple {:subject (propertyTranslation/translate s)
                :predicate "rdf:type"
                :object "owl:SymmetricProperty"
                :annotation (ann/translate annotation)}]
    triple))

(defn translateTransitiveProperty
  "Translate TransitiveObjectProperty"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op s] owl
        triple {:subject (propertyTranslation/translate s)
                :predicate "rdf:type"
                :object "owl:TransitiveProperty"
                :annotation (ann/translate annotation)}]
    triple))

(defn translateTwoDisjointProperties
  "Translate DisjointProperties"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op s o] owl
        triple {:subject (propertyTranslation/translate s)
                :predicate "owl:propertyDisjointWith"
                :object (propertyTranslation/translate o)
                :annotation (ann/translate annotation)}]
    triple))

;TODO: check translation for thick2ofn first
(defn translateAllDisjointProperties
  "Translate DisjointProperties"
  [ofn]
  (let [annotation (ann/get-annotation ofn)
        owl (ann/get-owl ofn)
        [_op & s] owl
        triple {:subject (gensym "_:genid")
                :predicate "owl:AllDisjointProperties"
                :object {:members (translateList s)}
                :annotation (ann/translate annotation)}]
    triple))

(defn translateDisjointProperties
  "Translate DisjointProperties"
  [ofn]
  (let [owl (ann/get-owl ofn)
        [_operator & arguments] owl]
    (if (= 2 (count arguments))
      (translateTwoDisjointProperties ofn)
      (translateAllDisjointProperties ofn))))

(defn translate
  "Translate OFN-S expression to thick triple"
  [ofn]
  (let [operator (first ofn)];
    ;(println predicateMap)
    (case operator
      "SubObjectPropertyOf" (translateSubObjectPropertyOf ofn)
      ;"DisjointObjectProperties" (translateDisjointProperties ofn)
      "ObjectPropertyDomain" (translateDomain ofn)
      "ObjectPropertyRange" (translateRange ofn)
      "InverseObjectProperties" (translateInverseOf ofn)

      "FunctionalObjectProperty" (translateFunctionalProperty ofn)
      "InverseFunctionalObjectProperty" (translateInverseFunctionalProperty ofn)
      "ReflexiveObjectProperty" (translateReflexiveProperty ofn)
      "IrreflexiveObjectProperty" (translateIrreflexiveProperty ofn)
      "AsymmetricObjectProperty" (translateAsymmetricProperty ofn)
      "SymmetricObjectProperty" (translateSymmetricProperty ofn)
      "TransitiveObjectProperty" (translateTransitiveProperty ofn)
      "DisjointObjectProperties" (translateDisjointProperties ofn)
      "DisjointProperties" (translateDisjointProperties ofn))))
