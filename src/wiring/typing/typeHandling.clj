(ns wiring.typing.typeHandling
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [wiring.util.thickTriples :as thickTriples]
            [wiring.util.ofnExpressions :as ofnExpressions]))

(declare translate)

(defn typingTriple?
  "Check whether an OFN-S expression contains a label"
  [ofn]
   (and (= (first ofn) "ThinTriple")
        (= (nth ofn 2) "rdf:type"))) 

(defn updateSubject2info
  "Adds an element to a map."
  [subject2types typeExpression]
  (let [subject (second typeExpression)
        info (nth typeExpression 3)]
    (if (contains? subject2types subject)
      (assoc subject2types subject (conj (get subject2types subject) info))
      (assoc subject2types subject #{info})))) 

(defn extractTyping
  "Takes a colelction of OFN-S expressions and returns a map for labels"
  [ofns]
  (let [types (filter typingTriple? ofns)
        subject2types (reduce updateSubject2info (hash-map) types)]
  subject2types))

(defn id
  "Identity function for OFN-S expressions."
  [ofn subject2type]
  (if (ofnExpressions/namedEntity? ofn)
    ofn
    (let [[operator & arguments] ofn
           args (map #(translate % subject2type) arguments)
           op (vec (cons operator args))]
      op))) 

(defn translateSomeValuesFrom 
  "Translate an untyped existential expression."
  [ofn subject2type]
  (let [[op property filler] ofn
        classExpression (or (contains? (get subject2type property) "owl:ObjectProperty")
                            (contains? (get subject2type filler) "owl:Class"))
        datatypeExpression (or (contains? (get subject2type property) "owl:DatatypeProperty")
                               (contains? (get subject2type filler) "rdfs:Datatype"))]
    (cond classExpression (vector "ObjectSomeValuesFrom" property filler)
          datatypeExpression (vector "DataSomeValuesFrom" property filler)
          :else (vector "ErrorSomeValuesFrom" property filler))))

(defn translateAllValuesFrom 
  "Translate an untyped universal expression."
  [ofn subject2type]
  (let [[op property filler] ofn
        classExpression (or (contains? (get subject2type property) "owl:ObjectProperty")
                            (contains? (get subject2type filler) "owl:Class"))
        datatypeExpression (or (contains? (get subject2type property) "owl:DatatypeProperty")
                               (contains? (get subject2type filler) "rdfs:Datatype"))]
    (cond classExpression (vector "ObjectAllValuesFrom" property filler)
          datatypeExpression (vector "DataAllValuesFrom" property filler)
          :else (vector "ErrorAllValuesFrom" property filler))))

(defn translateHasValue
  "Translate an untyped hasValue expression."
  [ofn subject2type]
  (let [[op property filler] ofn;filler is a literal or an individual
        classExpression (or  (contains? (get subject2type property) "owl:ObjectProperty")
                            (contains? (get subject2type filler) "owl:NamedIndividual"))
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")]
    (cond classExpression (vector "ObjectHasValue" property filler)
          datatypeExpression (vector "DataHasValue" property filler)
          :else (vector "ErrorHasValue" property filler))))

(defn translateMinCardinality
  "Translate an untyped MinCardinality expression."
  [ofn subject2type]
  (let [[op cardinality property] ofn
        classExpression (contains? (get subject2type property) "owl:ObjectProperty")
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")]
    (cond classExpression (vector "ObjectMinCardinality" cardinality property)
          datatypeExpression (vector "DataMinCardinality" cardinality property)
          :else (vector "ErrorMinCardinality" cardinality property))))

(defn translateMaxCardinality
  "Translate an untyped MaxCardinality expression."
  [ofn subject2type]
  (let [[op cardinality property] ofn
        classExpression (contains? (get subject2type property) "owl:ObjectProperty")
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")]
    (cond classExpression (vector "ObjectMaxCardinality" cardinality property)
          datatypeExpression (vector "DataMaxCardinality" cardinality property)
          :else (vector "ErrorMaxCardinality" cardinality property))))

(defn translateExactCardinality
  "Translate an untyped ExactCardinality expression."
  [ofn subject2type]
  (let [[op cardinality property] ofn
        classExpression (contains? (get subject2type property) "owl:ObjectProperty")
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")]
    (cond classExpression (vector "ObjectExactCardinality" cardinality property)
          datatypeExpression (vector "DataExactCardinality" cardinality property)
          :else (vector "ErrorExactCardinality" cardinality property)))) 

;gets an OFN S-Exression and a typing map 
(defn translate
  "Translate (abstract) OFN-S expression to an OFN-S expression"
  [ofn subject2type]
  (let [operator (first ofn)];
    (case operator 
      ;;assume that we  only need to look up type information for named entities
      "SomeValuesFrom"  (translateSomeValuesFrom ofn subject2type)
      "AllValuesFrom"  (translateAllValuesFrom ofn subject2type)
      "HasValue"  (translateHasValue ofn subject2type) 
      "MinCardinality"  (translateMinCardinality ofn subject2type)
      "MaxCardinality"  (translateMaxCardinality ofn subject2type)
      "ExactCardinality"  (translateExactCardinality ofn subject2type)
      ;assume these are typed correctly (for now)
      ;"ObjectIntersectionOf"  (translateObjectIntersection ofn)
      ;"ObjectUnionOf"  (translateObjectUnion ofn)
      ;"ObjectOneOf"  (translateObjectOneOf ofn)
      ;"ObjectComplementOf"  (translateObjectComplement ofn)
      ;"ObjectHasSelf"  (translateObjectHasSelf ofn)
      (id ofn subject2type)))) 

(defn translateSet
  "Translate a set of (abstract) OFN-S expressions"
  [ofns subject2types]
  (map #(translate % subject2types) ofns)) 

(defn typingUpdate
  "Takes an ontology represented by OFN-S expressions and
  updates their type according to available typing information "
  [ontology]
  (let [subject2types (extractTyping ontology)
        typing (translateSet ontology subject2types)]
    typing))
