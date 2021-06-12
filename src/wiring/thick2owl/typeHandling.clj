(ns wiring.thick2owl.typeHandling
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]
            [wiring.util.ofnExpressions :as ofnExpressions]
            [wiring.thick2ofn.util :as ofnUtil];TODO: refactor this
            [clojure.spec.alpha :as spec]))

(declare translate)
;TODO use ofnFormat

(defn id
  "Identity function for OFN-S expressions."
  [ofn subject2type]
  (if (ofnExpressions/namedEntity? ofn)
    (str \" ofn \")
    (let [[operator & arguments] ofn
           args (map #(translate % subject2type) arguments)
           op (ofnUtil/ofsFormat operator (apply ofnUtil/ofsFormatNoBrackets args))]
      op))) 

(defn translateSomeValuesFrom 
  "Translate an untyped existential expression."
  [ofn subject2type]
  (let [[op property filler] ofn
        classExpression (or (contains? (get subject2type property) "owl:ObjectProperty")
                            (contains? (get subject2type filler) "owl:Class"))
        datatypeExpression (or (contains? (get subject2type property) "owl:DatatypeProperty")
                               (contains? (get subject2type filler) "rdfs:Datatype"))
        property (str "\"" property "\""); wrap internal representation into strings for JSON
        filler (str "\"" filler "\"")]
    (cond classExpression (ofnUtil/ofsFormat "ObjectSomeValuesFrom" property filler)
          datatypeExpression (ofnUtil/ofsFormat "DataSomeValuesFrom" property filler)
          :else (ofnUtil/ofsFormat "ErrorSomeValuesFrom" property filler))))

(defn translateAllValuesFrom 
  "Translate an untyped universal expression."
  [ofn subject2type]
  (let [[op property filler] ofn
        classExpression (or (contains? (get subject2type property) "owl:ObjectProperty")
                            (contains? (get subject2type filler) "owl:Class"))
        datatypeExpression (or (contains? (get subject2type property) "owl:DatatypeProperty")
                               (contains? (get subject2type filler) "rdfs:Datatype"))
        property (str "\"" property "\""); wrap internal representation into strings for JSON
        filler (str "\"" filler "\"")]
    (cond classExpression (ofnUtil/ofsFormat "ObjectAllValuesFrom" property filler)
          datatypeExpression (ofnUtil/ofsFormat "DataAllValuesFrom" property filler)
          :else (ofnUtil/ofsFormat "ErrorAllValuesFrom" property filler))))

(defn translateHasValue
  "Translate an untyped hasValue expression."
  [ofn subject2type]
  (let [[op property filler] ofn;filler is a literal or an individual
        classExpression (or  (contains? (get subject2type property) "owl:ObjectProperty")
                            (contains? (get subject2type filler) "owl:NamedIndividual"))
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")
        property (str "\"" property "\""); wrap internal representation into strings for JSON
        filler (str "\"" filler "\"")] 
    (cond classExpression (ofnUtil/ofsFormat "ObjectHasValue" property filler)
          datatypeExpression (ofnUtil/ofsFormat "DataHasValue" property filler)
          :else (ofnUtil/ofsFormat "ErrorHasValue" property filler))))

(defn translateMinCardinality
  "Translate an untyped MinCardinality expression."
  [ofn subject2type]
  (let [[op cardinality property] ofn
        classExpression (contains? (get subject2type property) "owl:ObjectProperty")
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")
        property (str "\"" property "\""); wrap internal representation into strings for JSON
        cardinality (str "\"" cardinality "\"")] 
    (cond classExpression (ofnUtil/ofsFormat "ObjectMinCardinality" cardinality property)
          datatypeExpression (ofnUtil/ofsFormat "DataMinCardinality" cardinality property)
          :else (ofnUtil/ofsFormat "ErrorMinCardinality" cardinality property))))

(defn translateMaxCardinality
  "Translate an untyped MaxCardinality expression."
  [ofn subject2type]
  (let [[op cardinality property] ofn
        classExpression (contains? (get subject2type property) "owl:ObjectProperty")
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")
        property (str "\"" property "\""); wrap internal representation into strings for JSON
        cardinality (str "\"" cardinality "\"")] 
    (cond classExpression (ofnUtil/ofsFormat "ObjectMaxCardinality" cardinality property)
          datatypeExpression (ofnUtil/ofsFormat "DataMaxCardinality" cardinality property)
          :else (ofnUtil/ofsFormat "ErrorMaxCardinality" cardinality property))))

(defn translateExactCardinality
  "Translate an untyped ExactCardinality expression."
  [ofn subject2type]
  (let [[op cardinality property] ofn
        classExpression (contains? (get subject2type property) "owl:ObjectProperty")
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")
        property (str "\"" property "\""); wrap internal representation into strings for JSON
        cardinality (str "\"" cardinality "\"")] 
    (cond classExpression (ofnUtil/ofsFormat "ObjectExactCardinality" cardinality property)
          datatypeExpression (ofnUtil/ofsFormat "DataExactCardinality" cardinality property)
          :else (ofnUtil/ofsFormat "ErrorExactCardinality" cardinality property)))) 

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
