(ns wiring.ldtab2ofn.expressionTranslation.classTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.ldtab2ofn.util :as util]
            [wiring.ldtab2ofn.expressionTranslation.propertyTranslation :as property]
            [wiring.ldtab2ofn.expressionTranslation.dataTypeTranslation :as DTT]
            [wiring.ldtab2ofn.typeInference :as typeInference]
            [wiring.ldtab2ofn.spec :as owlspec]))

(declare translate) ;recursive translation (not tail recursive)  

;TODO put this in util?
(defn curry-predicate-map
  [predicateMap]
  (fn [x] (-> predicateMap
              (get x)
              first
              :object)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Type Inference Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The following two functions are used to infer the type of some expressions
;For example an existential restriciton can be either an
;ObjectSomeValuesFrom or a DataSomeValuesFrom.
;If we can determine whether the used property expression or the filler
;is an ObjectProperty (DataProperty) or a Class (Datatype),
;then, we can infer the type of the expression itself.  


(defn is-class-expression?
  "Checks whether an expression is a non-atomic OWL class expression."
  [expression]
  (case (first expression)
    "ObjectSomeValuesFrom" true
    "ObjectAllValuesFrom" true
    "ObjectHasValue" true
    "ObjectMinCardinality" true
    "ObjectMaxCardinality" true
    "ObjectExactCardinality" true
    "ObjectIntersectionOf" true
    "ObjectUnionOf" true
    "ObjectOneOf" true
    "ObjectComplementOf" true
    "ObjectHasSelf" true
      ;TODO Data expressions
      ;ambiguous
    "SomeValuesFrom" true
    "AllValuesFrom" true
    "HasValue" true
    "MaxCardinality" true
    "MinCardinality" true
    "ExactCardinality" true
    false))

(defn is-typed-as-class?
  "Checks whether a predicate map is typed as an OWL class expression."
  [expression]
  (if (and (map? expression)
           (contains? expression :rdf:type))
    (or (= "owl:Class" (:rdf:type expression))
        (= "owl:Restriction" (:rdf:type expression)));Restrictions are class expressions
    false));could still be a named class expression - but we can't find that out without type information

(defn is-typed-as-object-property?
  "Checks whether a predicate map is typed as an OWL object property expression."
  [expression]
  (and (map? expression)
       (contains? expression :owl:inverseOf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      RDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NB: these are lists for class expressions (due to the local recursive call)
(defn translateList
  "Translate an RDF list."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/list predicates)]}
  (loop [in predicates
         out []]
    (if (= in "rdf:nil")
      out
      (recur (:object (first (:rdf:rest in)))
             (conj out (translate (:object (first (:rdf:first in)))))))));recursively translate class expressions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Restrictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;since we don't have type information available in thick triples
;we use an abstraction that will be translated in a second step
(defn translateExistentialRestriction
  "Translate an existential restriction."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/existential predicates)]}
  (let [get-object (curry-predicate-map predicates)
        onProperty (property/translate (get-object :owl:onProperty))
        filler (translate (get-object :owl:someValuesFrom))
        rawProperty (get-object :owl:onProperty)
        rawFiller (get-object :owl:someValuesFrom)]
    (cond (or (is-typed-as-class? rawFiller)
              (is-typed-as-object-property? rawProperty)
              (is-class-expression? filler)) (vector "ObjectSomeValuesFrom" onProperty filler)
          (or (typeInference/is-typed-as-datatype? rawFiller)
              (typeInference/is-data-range-expression? filler)) (vector "DataSomeValuesFrom" onProperty filler)
          :else (vector "SomeValuesFrom" onProperty filler))))

(defn translateUniversalRestriction
  "Translate a universal restriction."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/universal predicates)]}
  (let [get-object (curry-predicate-map predicates)
        onProperty (property/translate (get-object :owl:onProperty))
        filler (translate (get-object :owl:allValuesFrom))
        rawProperty (get-object :owl:onProperty)
        rawFiller (get-object :owl:someValuesFrom)]
    (cond (or (is-typed-as-class? rawFiller)
              (is-typed-as-object-property? rawProperty)
              (is-class-expression? filler)) (vector "ObjectAllValuesFrom" onProperty filler)
          (or (typeInference/is-typed-as-datatype? rawFiller)
              (typeInference/is-data-range-expression? filler)) (vector "DataSomeValuesFrom" onProperty filler)
          :else (vector "AllValuesFrom" onProperty filler))))

(defn translateHasValueRestriction
  "Translate hasValue restriction."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/hasValue predicates)]}
  (let [get-object (curry-predicate-map predicates)
        onProperty (property/translate (get-object :owl:onProperty))
        rawProperty (get-object :owl:onProperty)
        filler (get-object :owl:hasValue)];individual
    (if (is-typed-as-object-property? rawProperty)
      (vector "ObjectHasValue" onProperty filler);type inference
      (vector "HasValue" onProperty filler))))

(defn translateHasSelfRestriction
  "Translate hasSelf restriction."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/hasSelf predicates)]}
  (let [get-object (curry-predicate-map predicates)
        onProperty (property/translate (get-object :owl:onProperty))]
    (vector "ObjectHasSelf" onProperty)));this has no datatype variant


;NB: qualified cardinality restrictions are necessarily typed correctly
;due to "owl:onRange" and "owl:onClass" 
(defn translateMinCardinalityRestriction
  "Translate minimum cardinality restriction."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/minCardinality predicates)]}
  (let [get-object (curry-predicate-map predicates)
        onProperty (property/translate (get-object :owl:onProperty))
        rawProperty (get-object :owl:onProperty)
        cardinality (get-object :owl:minCardinality)]
    (if (is-typed-as-object-property? rawProperty)
      (vector "ObjectMinCardinality" cardinality onProperty)
      (vector "MinCardinality" cardinality onProperty))))

(defn translateMinQualifiedCardinalityRestriction
  "Translate minimum qualified cardinality restriction."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/minQualifiedCardinality predicates)]}
  (let [get-object (curry-predicate-map predicates)
        onProperty (property/translate (get-object :owl:onProperty))
        cardinality (get-object :owl:minQualifiedCardinality)
        filler (translate (get-object :owl:onClass))]
    (vector "ObjectMinCardinality" cardinality onProperty filler)))

(defn translateMaxCardinalityRestriction
  "Translate maximum cardinality restriction."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/maxCardinality predicates)]}
  (let [get-object (curry-predicate-map predicates)
        onProperty (property/translate (get-object :owl:onProperty))
        rawProperty (get-object :owl:onProperty)
        cardinality (get-object :owl:maxCardinality)];
    (if (is-typed-as-object-property? rawProperty)
      (vector "ObjectMaxCardinality" cardinality onProperty)
      (vector "MaxCardinality" cardinality onProperty))))

(defn translateMaxQualifiedCardinalityRestriction
  "Translate maximum qualified cardinality restriction."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/maxQualifiedCardinality predicates)]}
  (let [get-object (curry-predicate-map predicates)
        onProperty (property/translate (get-object :owl:onProperty))
        cardinality (util/getNumber (get-object :owl:maxQualifiedCardinality))
        filler (translate (get-object :owl:onClass))];
    (vector "ObjectMaxCardinality" cardinality onProperty filler)))

(defn translateExactCardinalityRestriction
  "Translate exact cardinality restriction."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/exactCardinality predicates)]}
  (let [get-object (curry-predicate-map predicates)
        onProperty (property/translate (get-object :owl:onProperty))
        rawProperty (get-object :owl:onProperty)
        cardinality (get-object :owl:cardinality)];
    (if (is-typed-as-object-property? rawProperty)
      (vector "ObjectExactCardinality" cardinality onProperty)
      (vector "ExactCardinality" cardinality onProperty))))

(defn translateExactQualifiedCardinalityRestriction
  "Translate exact qualified cardinality restriction."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/exactQualifiedCardinality predicates)]}
  (let [get-object (curry-predicate-map predicates)
        onProperty (property/translate (get-object :owl:onProperty))
        cardinality (util/getNumber (get-object :owl:qualifiedCardinality))
        filler (translate (get-object :owl:onClass))];
    (vector "ObjectExactCardinality" cardinality onProperty filler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   OWL classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateIntersection
  "Translate an intersection."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/classIntersection predicates)]}
  (let [get-object (curry-predicate-map predicates)
        arguments (translate (get-object :owl:intersectionOf))]
    (cond (or (some is-class-expression? arguments)
              (is-typed-as-class? predicates)) (vec (cons "ObjectIntersectionOf" arguments))
          (or (some typeInference/is-data-range-expression? arguments)
              (typeInference/is-typed-as-datatype? predicates)) (vec (cons "DataIntersectionOf" arguments))
          :else (vec (cons "IntersectionOf" arguments)))))

(defn translateUnion
  "Translate a union."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/classUnion predicates)]}
  (let [get-object (curry-predicate-map predicates)
        arguments (translate (get-object :owl:unionOf))]
    (cond (or (some is-class-expression? arguments)
              (is-typed-as-class? predicates)) (vec (cons "ObjectUnionOf" arguments))
          (or (some typeInference/is-data-range-expression? arguments)
              (typeInference/is-typed-as-datatype? predicates)) (vec (cons "DataUnionOf" arguments))
          :else (vec (cons "UnionOf" arguments)))))

(defn translateOneOf
  "Translate a oneOf."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/oneOf predicates)]}
  (let [get-object (curry-predicate-map predicates)
        arguments (translateList (get-object :owl:oneOf))]
    (vec (cons "OneOf" arguments))));TODO need to identify types of individuals (ObjectOneOf) & literlas (DataOneOf)

(defn translateComplement
  "Translate complement."
  [predicates]
  ;{:pre [(spec/valid? ::owlspec/classComplement predicates)]}
  (let [get-object (curry-predicate-map predicates)
        argument (translate (get-object :owl:complementOf))]
    (cond (or (is-class-expression? argument)
              (is-typed-as-class? predicates)) (vector "ObjectComplementOf" argument)
          (or (typeInference/is-data-range-expression? argument)
              (typeInference/is-typed-as-datatype? predicates)) (vector "DataComplementOf" argument)
          :else (vector "ComplementOf" argument))))

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
    (and (contains? predicates :owl:minQualifiedCardinality)
         (contains? predicates :owl:onClass)) (translateMinQualifiedCardinalityRestriction predicates)
    (and (contains? predicates :owl:minQualifiedCardinality)
         (contains? predicates :owl:onDataRange)) (DTT/translateDatatypeMinQualifiedCardinality predicates)
    (contains? predicates :owl:maxCardinality) (translateMaxCardinalityRestriction predicates)
    (and (contains? predicates :owl:maxQualifiedCardinality)
         (contains? predicates :owl:onClass)) (translateMaxQualifiedCardinalityRestriction predicates)
    (and (contains? predicates :owl:maxQualifiedCardinality)
         (contains? predicates :owl:onDataRange)) (DTT/translateDatatypeMaxQualifiedCardinality predicates)
    (contains? predicates :owl:cardinality) (translateExactCardinalityRestriction predicates)
    (and (contains? predicates :owl:qualifiedCardinality)
         (contains? predicates :owl:onClass)) (translateExactQualifiedCardinalityRestriction predicates)
    (and (contains? predicates :owl:qualifiedCardinality)
         (contains? predicates :owl:onDataRange)) (DTT/translateDatatypeExactQualifiedCardinality predicates)))

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

;Note: this should only be called for RDF lists because that's the only
;predicate map that doesn't come with a type (and is not a named entity)
;but it would also be possible to have missing type information in predicate maps
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
  (let [entrypoint (:object (first (:rdf:type predicates)))]
    ;(println "typesd")
    (case entrypoint
      "owl:Restriction" (translateRestriction predicates)
      "owl:Class" (translateClass predicates)
      "rdfs:Datatype" (DTT/translate predicates)
      "")))

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (cond
    (string? predicateMap) predicateMap;base case
    (util/typed? predicateMap) (translateTyped predicateMap);type information is available
    :else (translateUntyped predicateMap)));type information is not available
