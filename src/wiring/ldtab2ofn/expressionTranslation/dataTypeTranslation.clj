(ns wiring.ldtab2ofn.expressionTranslation.dataTypeTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            ;[wiring.ldtab2ofn.expressionTranslation.classTranslation :as CET]
            [wiring.ldtab2ofn.typeInference :as typeInference]
            [wiring.ldtab2ofn.util :as util]
            [wiring.ldtab2ofn.spec :as owlspec]))

(declare translate) ;recursive translation (not tail recursive)  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      RDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NB: these are lists for data types (due to the local recursive call)
(defn translateList
  "Translate an RDF list."
  [predicates]
  {:pre [(spec/valid? ::owlspec/list predicates)]
   :post [(spec/valid? string? %)]}
  (loop [in predicates
         out []]
    (if (= in "rdf:nil")
      out ;(apply util/ofsFormatNoBrackets out)
      (recur (:rdf:rest in)
             (conj out (translate (:rdf:first in)))))));recursively translate data types

(defn translateFacet
  "Translate a datatype facet."
  [predicates]
  ;a facet is a map of the form {:facet :constraint} 
  (let [facet (name (first (keys predicates)))
        constraint (first (vals predicates))]
    (vector "Facet" facet constraint)))

(defn translateRestrictions
  "Translate an RDF list of restrictions."
  [predicates]
  (loop [in predicates
         out []]
    (if (= in "rdf:nil")
      out ;(apply util/ofsFormatNoBrackets out)
      (recur (:rdf:rest in)
             (conj out (translateFacet (:rdf:first in)))))));recursively translate data types


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Data Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateIntersection
  "Translate an intersection."
  [predicates]
  {:pre [(spec/valid? ::owlspec/classIntersection predicates)]}
  (let [arguments (translate (:owl:intersectionOf predicates))]
    (cond (or (some typeInference/is-class-expression? arguments)
              (typeInference/is-typed-as-class? predicates)) (vec (cons "ObjectIntersectionOf" arguments))
          (or (some typeInference/is-data-range-expression? arguments)
              (typeInference/is-typed-as-datatype? predicates)) (vec (cons "DataIntersectionOf" arguments))
          :else (vec (cons "IntersectionOf" arguments)))))

(defn translateDatatypeUnion
  "Translate datatype union"
  [predicates]
  (let [arguments (translate (:owl:unionOf predicates))]
    (vec (cons "DataUnionOf" arguments))))

(defn translateDatatypeComplement
  "Translate datatype complement"
  [predicates]
  (let [dataRange (translate (:owl:datatypeComplementOf predicates))]
    (vector "DataComplementOf" dataRange)))

(defn translateDatatypeOneOf
  "Translate datatype one of"
  [predicates]
  (let [arguments (translateList (:owl:oneOf predicates))]
    (vec (cons "DataOneOf" arguments))))

;;ASSUMPTION: the arguments of the list are strings 
(defn translateDatatypeRestriction
  "Translate datatype restriction"
  [predicates]
  (let [datatype (translate (:owl:onDatatype predicates))
        arguments (translateRestrictions (:owl:withRestrictions predicates))]
    (vec (cons "DatatypeRestriction" (cons datatype arguments)))))

(defn translateDatatypeExistential
  "Translate datatype existential"
  [predicates]
  (let [property (translate (:owl:onProperty predicates));this can be a list
        dataRange (translate (:owl:someValuesFrom predicates))]
    (vector "DataSomeValuesFrom" property dataRange)))

(defn translateDatatypeUniversal
  "Translate datatype universal"
  [predicates]
  (let [property (translate (:owl:onProperty predicates));this can be a list
        dataRange (translate (:owl:allValuesFrom predicates))]
    (vector "DataAllValuesFrom" property dataRange)))

(defn translateDatatypeHasValue
  "Translate datatype has value"
  [predicates]
  (let [property (translate (:owl:onProperty predicates));this can be a list
        literal (translate (:owl:hasValue predicates))]
    (vector "DataHasValue" property literal)))

(defn translateDatatypeMinCardinality
  "Translate datatype min cardinality"
  [predicates]
  (let [cardinality (util/getNumber (:owl:minCardinality predicates))
        property (translate (:owl:onProperty predicates))]
    (vector "DataMinCardinality" cardinality property)))

(defn translateDatatypeMinQualifiedCardinality
  "Translate datatype min qualified cardinality"
  [predicates]
  (let [cardinality (util/getNumber (:owl:minQualifiedCardinality predicates))
        property (translate (:owl:onProperty predicates))
        dataRange (translate (:owl:onDataRange predicates))]
    (vector "DataMinCardinality" cardinality property dataRange)))

(defn translateDatatypeMaxCardinality
  "Translate datatype max cardinality"
  [predicates]
  (let [cardinality (util/getNumber (:owl:maxCardinality predicates))
        property (translate (:owl:onProperty predicates))]
    (vector "DataMaxCardinality" cardinality property)))

(defn translateDatatypeMaxQualifiedCardinality
  "Translate datatype max qualified cardinality"
  [predicates]
  (let [cardinality (util/getNumber (:owl:maxQualifiedCardinality predicates))
        property (translate (:owl:onProperty predicates))
        dataRange (translate (:owl:onDataRange predicates))]
    (vector "DataMaxCardinality" cardinality property dataRange)))

(defn translateDatatypeExactCardinality
  "Translate datatype exact cardinality"
  [predicates]
  (let [cardinality (util/getNumber (:owl:cardinality predicates))
        property (translate (:owl:onProperty predicates))]
    (vector "DataExactCardinality" cardinality property)))

(defn translateDatatypeExactQualifiedCardinality
  "Translate datatype exact qualified cardinality"
  [predicates]
  (let [cardinality (util/getNumber (:owl:qualifiedCardinality predicates))
        property (translate (:owl:onProperty predicates))
        dataRange (translate (:owl:onDataRange predicates))]
    (vector "DataExactCardinality" cardinality property dataRange)))

(defn translateRestriction [predicates]
  (cond
    (contains? predicates :owl:someValuesFrom) (translateDatatypeExistential predicates)
    (contains? predicates :owl:allValuesFrom) (translateDatatypeUniversal predicates)
    (contains? predicates :owl:hasValue) (translateDatatypeHasValue predicates)
    (contains? predicates :owl:minCardinality) (translateDatatypeMinCardinality predicates)
    (contains? predicates :owl:minQualifiedCardinality) (translateDatatypeMinQualifiedCardinality predicates)
    (contains? predicates :owl:maxCardinality) (translateDatatypeMaxCardinality predicates)
    (contains? predicates :owl:maxQualifiedCardinality) (translateDatatypeMaxQualifiedCardinality predicates)
    (contains? predicates :owl:cardinality) (translateDatatypeExactCardinality predicates)
    (contains? predicates :owl:qualifiedCardinality) (translateDatatypeExactQualifiedCardinality predicates)))

(defn translateDatatype [predicates]
  "Translate an RDFS datatype expressions with an rdf:type."
  (cond
    (contains? predicates :owl:intersectionOf) (translateIntersection predicates)
    (contains? predicates :owl:unionOf) (translateDatatypeUnion predicates)
    (contains? predicates :owl:datatypeComplementOf) (translateDatatypeComplement predicates)
    (contains? predicates :owl:withRestrictions) (translateDatatypeRestriction predicates)
    (contains? predicates :owl:oneOf) (translateDatatypeOneOf predicates)))

(defn translateUntyped [predicates]
  "Translate expressions without rdf:type information."
  (let [datatypeAttempt (translateDatatype predicates);returns nil if no translation is performed
        restrictionAttempt (translateRestriction predicates)
        lis (contains? predicates :rdf:first)]
    (cond
      datatypeAttempt datatypeAttempt
      restrictionAttempt restrictionAttempt
      lis (translateList predicates)
      :else "")))

(defn translateTyped [predicates]
  "Translate expressions with an rdf:type."
  (let [entrypoint (:rdf:type predicates)]
    (case entrypoint
      "owl:Restriction" (translateRestriction predicates)
      "rdfs:Datatype" (translateDatatype predicates)
      "")));

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (cond
    (string? predicateMap) (str "\"" predicateMap "\"") ;base case
    (util/typed? predicateMap) (translateTyped predicateMap)
    :else (translateUntyped predicateMap)))
