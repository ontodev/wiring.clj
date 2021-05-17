(ns wiring.axiomTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.propertyTranslation :as propertyTranslation]
            [wiring.classTranslation :as classTranslation]
            [wiring.util :as util]
            [wiring.spec :as owlspec]))

;TODO data validation
(declare translate)

(defn translateSubclassOf 
  "Translate a SubClassOf axiom"   
  [predicates]
  (let [subclass (classTranslation/translate (:subject predicates))
        superclass (classTranslation/translate (:object predicates))]
    (util/ofsFormat "SubClassOf" subclass superclass)))

(defn translateDisjointUnionOf
  "Translate a Disjoint Union Of"
  [predicates]
  (let [subject (classTranslation/translate (:subject predicates))
        arguments (classTranslation/translate (:object predicates))]
    (util/ofsFormat "DisjointUnion" subject arguments)))

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (let [p (:predicate predicateMap)];
    (case p
    "rdfs:subClassOf" (translateSubclassOf predicateMap)
    "owl:disjointUnionOf" (translateDisjointUnionOf predicateMap))))
    ;TODO
    ;equivalence classes (here we would need to parse multiple triples?)
    ;disjointclasses (seems broken in gizmos)
