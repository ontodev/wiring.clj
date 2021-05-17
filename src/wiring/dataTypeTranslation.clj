(ns wiring.dataTypeTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.util :as util]
            [wiring.spec :as owlspec]))

(declare translate) ;recursive parsing (not tail recursive)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      RDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateList
  "Translate an RDF list."
  [predicates]
  {:pre [(spec/valid? ::owlspec/list predicates)]
   :post [(spec/valid? string? %)]}
  (loop [in predicates
         out []]
    (if (= in "rdf:nil")
      (apply ofsFormat out)
      (recur (:rdf:rest in)
             (conj out (translate (:rdf:first in)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Data Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateDatatypeIntersection
  "Translate datatype intersection"
  [predicates]
  (let [arguments (translate (:owl:intersectionOf predicates))] 
    (ofsFormat "DataIntersectionOf" arguments))) 

(defn translateDatatypeUnion
  "Translate datatype union"
  [predicates]
  (let [arguments (translate (:owl:unionOf predicates))] 
    (ofsFormat "DataUnionOf" arguments))) 

(defn translateDatatypeComplement
  "Translate datatype complement"
  [predicates]
  (let [dataRange (translate (:owl:datatypeComplementOf predicates))] 
    (ofsFormat "DataComplementOf" dataRange))) 

(defn translateDatatypeOneOf
  "Translate datatype one of"
  [predicates]
  (let [arguments (translate (:owl:oneOf predicates))] 
    (ofsFormat "DataOneOf" arguments))) 

;;ASSUMPTION: the arguments of the list are strings 
(defn translateDatatypeRestriction
  "Translate datatype restriction"
  [predicates]
  (let [datatype (translate (:owl:onDatatype predicates))
        arguments (translate (:owl:withRestrictions predicates))] 
    (ofsFormat "DatatypeRestriction" datatype arguments))) 

(defn translateDatatypeExistential
  "Translate datatype existential"
  [predicates]
  (let [property (translate (:owl:onProperty predicates));this can be a list
        dataRange (translate (:owl:someValuesFrom predicates))] 
    (ofsFormat "DataSomeValuesFrom" property dataRange))) 

(defn translateDatatypeUniversal
  "Translate datatype universal"
  [predicates]
  (let [property (translate (:owl:onProperty predicates));this can be a list
        dataRange (translate (:owl:allValuesFrom predicates))] 
    (ofsFormat "DataAllValuesFrom" property dataRange))) 

(defn translateDatatypeHasValue
  "Translate datatype has value"
  [predicates]
  (let [property (translate (:owl:onProperty predicates));this can be a list
        literal (translate (:owl:hasValue predicates))] 
    (ofsFormat "DataHasValue" property literal)))

(defn translateDatatypeMinCardinality
  "Translate datatype min cardinality"
  [predicates]
  (let [cardinality (getNumber (:owl:minCardinality predicates))
        property (translate (:owl:onProperty predicates))]
    (ofsFormat "DataMinCardinality" cardinality property)))

(defn translateDatatypeMinQualifiedCardinality
  "Translate datatype min qualified cardinality"
  [predicates]
  (let [cardinality (getNumber (:owl:minQualifiedCardinality predicates))
        property (translate (:owl:onProperty predicates))
        dataRange (translate (:owl:onDataRange predicates))]
    (ofsFormat "DataMinCardinality" cardinality property dataRange)))

(defn translateDatatypeMaxCardinality
  "Translate datatype max cardinality"
  [predicates]
  (let [cardinality (getNumber (:owl:maxCardinality predicates))
        property (translate (:owl:onProperty predicates))]
    (ofsFormat "DataMaxCardinality" cardinality property)))

(defn translateDatatypeMaxQualifiedCardinality
  "Translate datatype max qualified cardinality"
  [predicates]
  (let [cardinality (getNumber (:owl:maxQualifiedCardinality predicates))
        property (translate (:owl:onProperty predicates))
        dataRange (translate (:owl:onDataRange predicates))]
    (ofsFormat "DataMaxCardinality" cardinality property dataRange)))

(defn translateDatatypeExactCardinality
  "Translate datatype exact cardinality"
  [predicates]
  (let [cardinality (getNumber (:owl:cardinality predicates))
        property (translate (:owl:onProperty predicates))]
    (ofsFormat "DataExactCardinality" cardinality property)))

(defn translateDatatypeExactQualifiedCardinality
  "Translate datatype exact qualified cardinality"
  [predicates]
  (let [cardinality (getNumber (:owl:qualifiedCardinality predicates))
        property (translate (:owl:onProperty predicates))
        dataRange (translate (:owl:onDataRange predicates))]
    (ofsFormat "DataExactCardinality" cardinality property dataRange)))

(def translateRestriction [predicates]
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
    (contains? predicates :owl:intersectionOf) (translateDatatypeIntersection predicates)
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
      "rdfs:Datatype" (translateDatatype predicates))));

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (cond
    (string? predicateMap) predicateMap ;base case
    (util/typed? predicateMap) (translateTyped predicateMap)
    :else (translateUntyped predicateMap)))

