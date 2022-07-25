(ns wiring.ofn2ldtab.axiomTranslation.objectPropertyAxioms
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2ldtab.expressionTranslation.classTranslation :as propertyTranslation]
            [wiring.ofn2ldtab.expressionTranslation.classTranslation :as classTranslation]
            [wiring.ofn2ldtab.spec :as owlspec])
  (:gen-class))

(defn translateList
  "Translate class expressions into an RDF list"
  [expressions]
  (loop [in (reverse expressions);constructing list from last element to first
         out "rdf:nil"]
    (if (empty? in)
      out
      (recur (rest in)
             {:rdf:first [{:object (propertyTranslation/translate (first in)) }]
              :rdf:rest [{:object out}]}))))

;TODO recursive property translation
(defn translateSubObjectPropertyOf
  "Translate SubObjectPropertyOf"
  [ofn]
  (let [[op sub sup] ofn
    triple {:subject (propertyTranslation/translate sub)
            :predicate "rdfs:subPropertyOf"
            :object (propertyTranslation/translate sup)}]
    triple))

(defn translatePropertyChainAxiom
  "Translate ObjectPropertyChain"
  [ofn]
  (let [[op s o] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "owl:propertyChainAxiom"
            :object (propertyTranslation/translate o)}]
    triple))


(defn translateDomain
  "Translate property domain"
  [ofn]
  (let [[op s o] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "rdfs:domain"
            :object (classTranslation/translate o)}]
    triple))

(defn translateRange
  "Translate property range"
  [ofn]
  (let [[op s o] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "rdfs:range"
            :object (classTranslation/translate o)}]
    triple))

(defn translateInverseOf
  "Translate inverse of"
  [ofn]
  (let [[op s o] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "owl:inverseOf"
            :object (propertyTranslation/translate o)}]
    triple))

(defn translateFunctionalProperty
  "Translate FunctionalProperty"
  [ofn]
  (let [[op s] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "rdf:type"
            :object "owl:FunctionalProperty"}]
    triple))

(defn translateInverseFunctionalProperty
  "Translate InverseFunctionalProperty"
  [ofn]
  (let [[op s] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "rdf:type"
            :object "owl:InverseFunctionalProperty"}]
    triple))

(defn translateReflexiveProperty
  "Translate ReflexiveProperty"
  [ofn]
  (let [[op s] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "rdf:type"
            :object "owl:ReflexiveProperty"}]
    triple))

(defn translateIrreflexiveProperty
  "Translate IrreflexiveObjectProperty"
  [ofn]
  (let [[op s] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "rdf:type"
            :object "owl:IrreflexiveProperty"}]
    triple))

(defn translateAsymmetricProperty
  "Translate AsymmetricObjectProperty"
  [ofn]
  (let [[op s] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "rdf:type"
            :object "owl:AsymmetricProperty"}]
    triple))

(defn translateSymmetricProperty
  "Translate SymmetricObjectProperty"
  [ofn]
  (let [[op s] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "rdf:type"
            :object "owl:SymmetricProperty"}]
    triple))

(defn translateTransitiveProperty
  "Translate TransitiveObjectProperty"
  [ofn]
  (let [[op s] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "rdf:type"
            :object "owl:TransitiveProperty"}]
    triple))

(defn translateTwoDisjointProperties
  "Translate DisjointProperties"
  [ofn]
  (let [[op s o] ofn
    triple {:subject (propertyTranslation/translate s)
            :predicate "owl:propertyDisjointWith"
            :object (propertyTranslation/translate o)}]
    triple)) 

;TODO: check translation for thick2ofn first
(defn translateAllDisjointProperties 
  "Translate DisjointProperties"
  [ofn]
  (let [[_op & s] ofn
        triple {:subject (gensym "_:genid")
                :predicate "owl:AllDisjointProperties" 
                :object {:members (translateList s)}}]
    triple)) 

(defn translateDisjointProperties
  "Translate DisjointProperties"
  [ofn]
  (let [[_operator & arguments] ofn]
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
      "ObjectPropertyChain" (translatePropertyChainAxiom ofn)
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
