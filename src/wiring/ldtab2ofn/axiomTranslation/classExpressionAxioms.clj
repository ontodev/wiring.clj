(ns wiring.ldtab2ofn.axiomTranslation.classExpressionAxioms
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [cheshire.core :as cs] 
            [clojure.edn :as edn]
            [wiring.ldtab2ofn.expressionTranslation.propertyTranslation :as propertyTranslation]
            [wiring.ldtab2ofn.expressionTranslation.classTranslation :as classTranslation]
            [wiring.ldtab2ofn.spec :as owlspec]))

;TODO data validation for return values
(declare translate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Class Expression Axioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateSubclassOf
  "Translate a SubClassOf axiom"
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/thickTriple predicates)]}
  (let [subclass (classTranslation/translate (:subject predicates))
        superclass (classTranslation/translate (:object predicates))]
    (vector "SubClassOf" subclass superclass)))

(defn translateDisjointUnionOf
  "Translate a DisjointUnion axiom"
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/thickTriple predicates)]}
  (let [subject (classTranslation/translate (:subject predicates))
        arguments (classTranslation/translate (:object predicates))]
    (vec (cons "DisjointUnion" (cons subject arguments)))))

(defn translateAllDisjointClasses
  "Translate DisjointClasses axiom"
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/thickTriple predicates)]}
  ;TODO: here we'd need to adapet thigs already? to deal with this object nesting?
  (let [arguments (classTranslation/translate (:owl:members (:object predicates)))]
    (vec (cons "DisjointClasses" arguments))))

(defn translateEquivalentClasses
  "Translate Equivalent Class axiom"
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/thickTriple predicates)]}
  (let [subject (classTranslation/translate (:subject predicates))
        arguments (classTranslation/translate (:object predicates))
        nAry (and (map? (:object predicates))
                  (contains? (:object predicates) :rdf:first))]
    (if nAry
      (vec (cons "EquivalentClasses" arguments));EquivalentClasses(e1, ..., en)
      (vector "EquivalentClasses" subject arguments))));EquivalentClasses(e1,e2) 

(defn translateDisjointWith
  "Translate DisjointClasses axiom with only two operands."
  [predicates]
  (let [lhs (classTranslation/translate (:subject predicates))
        rhs (classTranslation/translate (:object predicates))]
    (vector "DisjointClasses" lhs rhs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (let [p (:predicate predicateMap)];
    (case p
      ;class expression axioms
      "rdfs:subClassOf" (translateSubclassOf predicateMap)
      "owl:disjointUnionOf" (translateDisjointUnionOf predicateMap)
      "owl:equivalentClass" (translateEquivalentClasses predicateMap)
      "owl:AllDisjointClasses" (translateAllDisjointClasses predicateMap)
      "owl:disjointWith" (translateDisjointWith predicateMap) 

      (str "Thin triple?" predicateMap))))
