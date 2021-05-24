(ns wiring.rdf2ofsObject
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  NB: early prototyping
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare translate) ;recursive parsing (not tail recursive)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn firstObject [predicates, predicate]
  "Given a prediate map, return the first 'object'."
  (->> predicates predicate first :object))

(defn getNumber [xsd]
  "Extract n from \"n\"^^xsd:nonNegativeInteger."
  (s/replace (first (s/split xsd #"\^")) #"\"" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              OWL restrictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO: nested property expressions

(defn translateExistentialRestriction [predicates]
  "Translate an existential restriction."
  (let [onProperty (firstObject predicates :owl:onProperty)
        filler (translate (firstObject predicates :owl:someValuesFrom))]
    (str "[\"ObjectSomeValuesFrom\",\"" onProperty "\",\"" filler "\"]")))

(defn translateUniversalRestriction [predicates]
  "Translate a universal restriction."
  (let [onProperty (firstObject predicates :owl:onProperty)
        filler (translate (firstObject predicates :owl:allValuesFrom))]
    (str "[\"ObjectAllValuesFrom\"," onProperty "," filler "]")))

(defn translateHasValueRestriction [predicates]
  "Translate hasValue restriction."
  (let [onProperty (firstObject predicates :owl:onProperty)
        filler (firstObject predicates :owl:hasValue)];individual
    (str "[\"ObjectHasValue\"," onProperty "," filler "]")))

(defn translateHasSelfRestriction [predicates]
  "Translate hasSelf restriction."
  (let [onProperty (firstObject predicates :owl:onProperty)]
    (str "[\"ObjectHasSelf\"," onProperty "]")))

(defn translateMinCardinalityRestriction [predicates]
  "Translate minimum cardinality restriction."
  (let [onProperty (firstObject predicates :owl:onProperty)
        cardinality (getNumber (firstObject predicates :owl:minCardinality))];
    (str "[\"ObjectMinCardinality\"," onProperty "," cardinality "]")))

(defn translateMinQualifiedCardinalityRestriction [predicates]
  "Translate minimum qualified cardinality restriction."
  (let [onProperty (firstObject predicates :owl:onProperty)
        cardinality (getNumber (firstObject predicates :owl:minQualifiedCardinality))
        filler (translate (firstObject predicates :owl:onClass))];
    (str "[\"ObjectMinCardinality\"," cardinality "," onProperty "," filler "]")))

(defn translateMaxCardinalityRestriction [predicates]
  "Translate maximum cardinality restriction."
  (let [onProperty (firstObject predicates :owl:onProperty)
        cardinality (getNumber (firstObject predicates :owl:maxCardinality))];
    (str "[\"ObjectMaxCardinality\"," onProperty "," cardinality "]")))

(defn translateMaxQualifiedCardinalityRestriction [predicates]
  "Translate maximum qualified cardinality restriction."
  (let [onProperty (firstObject predicates :owl:onProperty)
        cardinality (getNumber (firstObject predicates :owl:maxQualifiedCardinality))
        filler (translate (firstObject predicates :owl:onClass))];
    (str "[\"ObjectMaxCardinality\"," cardinality "," onProperty "," filler "]")))

(defn translateExactCardinalityRestriction [predicates]
  "Translate exact cardinality restriction."
  (let [onProperty (firstObject predicates :owl:onProperty)
        cardinality (getNumber (firstObject predicates :owl:cardinality))];
    (str "[\"ObjectExactCardinality\"," onProperty "," cardinality "]")))

(defn translateExactQualifiedCardinalityRestriction [predicates]
  "Translate exact qualified cardinality restriction."
  (let [onProperty (firstObject predicates :owl:onProperty)
        cardinality (getNumber (firstObject predicates :owl:qualifiedCardinality))
        filler (translate (firstObject predicates :owl:onClass))];
    (str "[\"ObjectExactCardinality\"," cardinality "," onProperty "," filler "]")))

(defn translateRestriction [predicates]
  "Translate an OWL restriction with an rdf:type."
  (cond
    (contains? predicates :owl:someValuesFrom) (translateExistentialRestriction predicates)
    (contains? predicates :owl:allValuesFrom) (translateUniversalRestriction predicates)
    (contains? predicates :owl:hasValue) (translateHasValueRestriction predicates)
    (contains? predicates :owl:hasSelf) (translateHasSelfRestriction predicates)
    (contains? predicates :owl:minCardinality) (translateMinCardinalityRestriction predicates)
    (contains? predicates :owl:minQualifiedCardinality) (translateMinQualifiedCardinalityRestriction predicates)
    (contains? predicates :owl:maxCardinality) (translateMaxCardinalityRestriction predicates)
    (contains? predicates :owl:maxQualifiedCardinality) (translateMaxQualifiedCardinalityRestriction predicates)
    (contains? predicates :owl:cardinality) (translateMaxCardinalityRestriction predicates)
    (contains? predicates :owl:qualifiedCardinality) (translateMaxQualifiedCardinalityRestriction predicates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      RDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateList [predicates]
  "Translate an RDF list."
  (loop [in predicates
         out ""]
    (if (= in "rdf:nil")
      (subs out 1) ;get rid of leading comma 
      (recur (firstObject in :rdf:rest)
             (str out ",\"" (translate (firstObject in :rdf:first)) "\"")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   OWL classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn translateIntersection [predicates]
  "Translate an intersection."
  (let [arguments (translate (firstObject predicates :owl:intersectionOf))]
    (str "[\"ObjectIntersectionOf\"," arguments "]")))

(defn translateUnion [predicates]
  "Translate a union."
  (let [arguments (translate (firstObject predicates :owl:unionOf))]
    (str "[\"ObjectUnionOf\"," arguments "]")))

(defn translateOneOf [predicates]
  "Translate a oneOf."
  (let [arguments (translateList (firstObject predicates :owl:oneOf))];TODO: no translation for individuals yet
    (str "[\"ObjectOneOf\"," arguments "]")))

(defn translateComplement [predicates]
  "Translate complement."
  (let [argument (translate (firstObject predicates :owl:complementOf))]
    (str "[\"ObjectComplementOf\"," argument "]")))

(defn translateClass [predicates]
  "Translate OWL class expressions with an rdf:type."
  (cond
    (contains? predicates :owl:intersectionOf) (translateIntersection predicates)
    (contains? predicates :owl:unionOf) (translateUnion predicates)
    (contains? predicates :owl:complementOf) (translateComplement predicates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   OWL classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateTyped [predicates]
  "Translate expressions with an rdf:type."
  (let [entrypoint (firstObject predicates :rdf:type)]
    (case entrypoint
      "owl:Restriction" (translateRestriction predicates)
      "owl:Class" (translateClass predicates)
      ;TODO
      ;rdfs:Datatype
      ;owl:AllDisjointProperties
      ;owl:allDifferent
      ;owl:NegativePropertyAssertion
      ;owl:FunctionalProperty..
      (translate entrypoint) ;default case for 'A rdf:type B'
      )))

(defn predicateMap? [predicates]
  (map? predicates))

(defn typed? [predicates]
  (contains? predicates :rdf:type))

;(defn translateTypedMapList [predicates]
;    ;;typed case
;  (let [rdfTypes (:rdf:type predicates)] ;what if something does not have an rdf: type?
;    (println rdfTypes)
;    (loop [todo rdfTypes; don't loop over predicates but over list of type
;           res []]; return a list of OFS-Expressions 
;      (if (empty? todo)
;        res
;        (recur (rest todo)
;               (conj res
;                     (translate (->> todo first :object) predicates)))))))


(defn translateUntyped [predicates]
  "Translate expressions without rdf:type information."
  (let [restriction (translateRestriction predicates) ;returns nil if no translation is performed
        clas (translateClass predicates) ;returns nil if no translation is performed
        lis (contains? predicates :rdf:first)]
    (cond
      restriction restriction
      clas clas
      lis (translateList predicates)
      :else "")))

(defn translate [predicates]
  "Translate RDF to OFS."
  (cond
    (not (predicateMap? predicates)) predicates ;base case - "ex:A" (which is not typed with rdf:type)
    (typed? predicates) (translateTyped predicates)
    :else (translateUntyped predicates)))

(defn predicateMap2OFS [predicateMap]
  "Given a predicate map (using commonly used namespace abbreviations, e.g. rdf and owl,),
    return an OWL Functional S-Expression.

    From
        {'rdf:type': [{'object': 'owl:Restriction'}],
         'owl:onProperty': [{'object': 'ex:part-of'}],
         'owl:someValuesFrom': [{'object': 'ex:bar'}]}
    to
        [\"ObjectSomeValuesFrom\", \"ex:part-of\", \"ex:bar\"]"
  (def predicates (cs/parse-string predicateMap true)) ; parse JSON as map
  ;(println predicates)
  ;(println (firstObject predicates :rdf:type))
  (println (translate predicates)))

