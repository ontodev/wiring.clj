(ns wiring.classTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.util :as util]
            [wiring.propertyTranslation :as property]
            [wiring.dataTypeTranslation :as dType]
            [wiring.spec :as owlspec]))

(declare translate) ;recursive translation (not tail recursive)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      RDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NB: these are lists for class expressions (due to the local recursive call)
(defn translateList
  "Translate an RDF list."
  [predicates]
  {:pre [(spec/valid? ::owlspec/list predicates)]
   :post [(spec/valid? string? %)]}
  (loop [in predicates
         out []]
    (if (= in "rdf:nil")
      (apply util/ofsFormatNoBrackets out)
      (recur (:rdf:rest in)
             (conj out (translate (:rdf:first in)))))));recursively translate class expressions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Restrictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateExistentialRestriction
  "Translate an existential restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/existential predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (property/translate (:owl:onProperty predicates))
        filler (translate (:owl:someValuesFrom predicates))]
    (util/ofsFormat "ObjectSomeValuesFrom" onProperty filler)))

(defn translateUniversalRestriction
  "Translate a universal restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/universal predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (property/translate (:owl:onProperty predicates))
        filler (translate (:owl:allValuesFrom predicates))]
    (util/ofsFormat "ObjectAllValuesFrom" onProperty filler)))

(defn translateHasValueRestriction
  "Translate hasValue restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/hasValue predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (property/translate (:owl:onProperty predicates))
        filler (:owl:hasValue predicates)];individual
    (util/ofsFormat "ObjectHasValue" onProperty filler)))

(defn translateHasSelfRestriction
  "Translate hasSelf restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/hasSelf predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (property/translate (:owl:onProperty predicates))]
    (util/ofsFormat "ObjectHasSelf" onProperty)))

(defn translateMinCardinalityRestriction
  "Translate minimum cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/minCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (property/translate (:owl:onProperty predicates))
        cardinality (util/getNumber (:owl:minCardinality predicates))];
    (util/ofsFormat "ObjectMinCardinality" cardinality onProperty)))

(defn translateMinQualifiedCardinalityRestriction
  "Translate minimum qualified cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/minQualifiedCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (property/translate (:owl:onProperty predicates))
        cardinality (util/getNumber (:owl:minQualifiedCardinality predicates))
        filler (translate (:owl:onClass predicates))];
    (util/ofsFormat "ObjectMinCardinality" cardinality onProperty filler)))

(defn translateMaxCardinalityRestriction
  "Translate maximum cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/maxCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (property/translate (:owl:onProperty predicates))
        cardinality (util/getNumber (:owl:maxCardinality predicates))];
    (util/ofsFormat "ObjectMaxCardinality" cardinality onProperty)))

(defn translateMaxQualifiedCardinalityRestriction
  "Translate maximum qualified cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/maxQualifiedCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (property/translate (:owl:onProperty predicates))
        cardinality (util/getNumber (:owl:maxQualifiedCardinality predicates))
        filler (translate (:owl:onClass predicates))];
    (util/ofsFormat "ObjectMaxCardinality" cardinality onProperty filler)))

(defn translateExactCardinalityRestriction
  "Translate exact cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/exactCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (property/translate (:owl:onProperty predicates))
        cardinality (util/getNumber (:owl:cardinality predicates))];
    (util/ofsFormat "ObjectExactCardinality" cardinality onProperty)))

(defn translateExactQualifiedCardinalityRestriction
  "Translate exact qualified cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/exactQualifiedCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (property/translate (:owl:onProperty predicates))
        cardinality (util/getNumber (:owl:qualifiedCardinality predicates))
        filler (translate (:owl:onClass predicates))];
    (util/ofsFormat "ObjectExactCardinality" cardinality onProperty filler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   OWL classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateIntersection
  "Translate an intersection."
  [predicates]
  {:pre [(spec/valid? ::owlspec/classIntersection predicates)]
   :post [(spec/valid? string? %)]}
  (let [arguments (translate (:owl:intersectionOf predicates))]
    (util/ofsFormat "ObjectIntersectionOf" arguments)))

(defn translateUnion
  "Translate a union."
  [predicates]
  {:pre [(spec/valid? ::owlspec/classUnion predicates)]
   :post [(spec/valid? string? %)]}
  (let [arguments (translate (:owl:unionOf predicates))]
    (util/ofsFormat "ObjectUnionOf" arguments)))

(defn translateOneOf
  "Translate a oneOf."
  [predicates]
  {:pre [(spec/valid? ::owlspec/oneOf predicates)]
   :post [(spec/valid? string? %)]}
  (let [arguments (translateList (:owl:oneOf predicates))];TODO: no translation for individuals yet
    (util/ofsFormat "ObjectOneOf" arguments)))

(defn translateComplement
  "Translate complement."
  [predicates]
  {:pre [(spec/valid? ::owlspec/classComplement predicates)]
   :post [(spec/valid? string? %)]}
  (let [argument (translate (:owl:complementOf predicates))]
    (util/ofsFormat "ObjectComplementOf" argument)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Translation by Case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn translateRestriction [predicates]
  (cond
    (contains? predicates :owl:someValuesFrom) (translateExistentialRestriction predicates)
    (contains? predicates :owl:allValuesFrom) (translateUniversalRestriction predicates)
    (contains? predicates :owl:hasValue) (translateHasValueRestriction predicates)
    (contains? predicates :owl:hasSelf) (translateHasSelfRestriction predicates)
    (contains? predicates :owl:minCardinality) (translateMinCardinalityRestriction predicates)
    (contains? predicates :owl:minQualifiedCardinality) (translateMinQualifiedCardinalityRestriction predicates)
    (contains? predicates :owl:maxCardinality) (translateMaxCardinalityRestriction predicates)
    (contains? predicates :owl:maxQualifiedCardinality) (translateMaxQualifiedCardinalityRestriction predicates)
    (contains? predicates :owl:cardinality) (translateExactCardinalityRestriction predicates)
    (contains? predicates :owl:qualifiedCardinality) (translateExactQualifiedCardinalityRestriction predicates)))

(defn translateClass [predicates]
  "Translate OWL class expressions with an rdf:type."
  (cond
    (contains? predicates :owl:intersectionOf) (translateIntersection predicates)
    (contains? predicates :owl:unionOf) (translateUnion predicates)
    (contains? predicates :owl:oneOf) (translateOneOf predicates)
    (contains? predicates :owl:complementOf) (translateComplement predicates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Translation Mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateUntyped [predicates]
  "Translate expressions without rdf:type information."
  (let [classAttempt (translateClass predicates);returns nil if no translation is performed
        restrictionAttempt (translateRestriction predicates)
        lis (contains? predicates :rdf:first)]
    (cond
      classAttempt classAttempt
      restrictionAttempt restrictionAttempt
      lis (translateList predicates)
      :else "")))

(defn translateTyped [predicates]
  "Translate expressions with an rdf:type."
  (let [entrypoint (:rdf:type predicates)]
    (case entrypoint
      "owl:Restriction" (translateRestriction predicates)
      "owl:Class" (translateClass predicates)
      "rdfs:Datatype" (dType/translate predicates))))

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (cond
    (string? predicateMap) (str "\"" predicateMap "\"") ;base case
    (util/typed? predicateMap) (translateTyped predicateMap)
    :else (translateUntyped predicateMap)))
