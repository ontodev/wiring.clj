(ns wiring.ldtab2ofn.util
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.ldtab2ofn.spec :as owlspec]))

(defn typed? [predicates]
  (contains? predicates :rdf:type))

(defn encode-entity
  ([entity] ;this is expected to be OFN
   ;(if (map? entity);so it should never be a map
     ;(encode-entity entity (:datatype entity))
     entity) 
  ([entity datatype];entity is OFN - datatype is from predicate map
   (cond
     (= datatype "_IRI") entity
     (= datatype "_JSON") entity 
     (= datatype "_plain") entity
     (str/starts-with? datatype "@") (str "\"" entity "\"" datatype)
     :else (str "\"" entity "\"" "^^" datatype))))
;
(defn getNumber [xsd]
  "Extract n from \"n\"^^xsd:nonNegativeInteger."
  (first (s/split xsd #"\^")))
  ;(str "\"" (first (s/split xsd #"\^")) "\""))

