(ns wiring.thick2owl.typeHandling
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]
            [wiring.util.ofnExpressions :as ofnExpressions]
            [clojure.spec.alpha :as spec]))

(declare translate)

(defn id
  "Identity function for OFN-S expressions."
  [ofn subject2type]
  (if (ofnExpressions/namedEntity? ofn)
    (str \" ofn \")
    (let [[operator & arguments] ofn
           args (map #(translate % subject2type) arguments)
           commas (interpose, "," args)
           stringArgs (apply str commas)
           op (str "[\"" operator "\"," stringArgs "]")]
      op))) 

(defn translateSomeValuesFrom 
  "Translate an untyped existential expression."
  [ofn subject2type]
  (let [[op property filler] ofn
        classExpression (or (contains? (get subject2type property) "owl:ObjectProperty")
                            (contains? (get subject2type filler) "owl:Class"))
        datatypeExpression (or (contains? (get subject2type property) "owl:DatatypeProperty")
                               (contains? (get subject2type filler) "rdfs:Datatype"))]
    (cond classExpression (str "[ObjectSomeValuesFrom,\"" property "\",\"" filler "\"]")
          datatypeExpression (str "[DataSomeValuesFrom,\"" property "\",\"" filler "\"]")
          :else (str "[ErrorSomeValuesFrom,\"" property "\",\"" filler "\"]"))))

(defn translateAllValuesFrom 
  "Translate an untyped universal expression."
  [ofn subject2type]
  (let [[op property filler] ofn
        classExpression (or (contains? (get subject2type property) "owl:ObjectProperty")
                            (contains? (get subject2type filler) "owl:Class"))
        datatypeExpression (or (contains? (get subject2type property) "owl:DatatypeProperty")
                               (contains? (get subject2type filler) "rdfs:Datatype"))]
    (cond classExpression (str "[ObjectAllValuesFrom,\"" property "\",\"" filler "\"]")
          datatypeExpression (str "[DataAllValuesFrom,\"" property "\",\"" filler "\"]")
          :else (str "[ErrorAllValuesFrom,\"" property "\",\"" filler "\"]"))))

(defn translateHasValue
  "Translate an untyped hasValue expression."
  [ofn subject2type]
  (let [[op property filler] ofn;filler is a literal or an individual
        classExpression (or  (contains? (get subject2type property) "owl:ObjectProperty")
                            (contains? (get subject2type filler) "owl:NamedIndividual"))
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")]
    (cond classExpression (str "[ObjectHasValue,\"" property "\",\"" filler "\"]")
          datatypeExpression (str "[DataHasValue,\"" property "\",\"" filler "\"]")
          :else (str "[ErrorHasValue,\"" property "\",\"" filler "\"]"))))

(defn translateMinCardinality
  "Translate an untyped MinCardinality expression."
  [ofn subject2type]
  (let [[op cardinality property] ofn
        classExpression (contains? (get subject2type property) "owl:ObjectProperty")
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")]
    (cond classExpression (str "[ObjectMinCardinality,\"" cardinality "\",\"" property "\"]")
          datatypeExpression (str "[DataMinCardinality,\"" cardinality "\",\"" property "\"]")
          :else (str "[ErrorMinCardinality,\"" cardinality "\",\"" property "\"]"))))

(defn translateMaxCardinality
  "Translate an untyped MaxCardinality expression."
  [ofn subject2type]
  (let [[op cardinality property] ofn
        classExpression (contains? (get subject2type property) "owl:ObjectProperty")
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")]
    (cond classExpression (str "[ObjectMaxCardinality,\"" cardinality "\",\"" property "\"]")
          datatypeExpression (str "[DataMaxCardinality,\"" cardinality "\",\"" property "\"]")
          :else (str "[ErrorMaxCardinality,\"" cardinality "\",\"" property "\"]"))))

(defn translateExactCardinality
  "Translate an untyped ExactCardinality expression."
  [ofn subject2type]
  (let [[op cardinality property] ofn
        classExpression (contains? (get subject2type property) "owl:ObjectProperty")
        datatypeExpression (contains? (get subject2type property) "owl:DatatypeProperty")]
    (cond classExpression (str "[ObjectExactCardinality,\"" cardinality "\",\"" property "\"]")
          datatypeExpression (str "[DataExactCardinality,\"" cardinality "\",\"" property "\"]")
          :else (str "[ErrorExactCardinality,\"" cardinality "\",\"" property "\"]")))) 

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
