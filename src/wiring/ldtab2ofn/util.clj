(ns wiring.ldtab2ofn.util
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.ldtab2ofn.spec :as owlspec]))

(defn typed? [predicates]
  (contains? predicates :rdf:type))

(defn getNumber [xsd]
  "Extract n from \"n\"^^xsd:nonNegativeInteger."
  (first (s/split xsd #"\^")))
  ;(str "\"" (first (s/split xsd #"\^")) "\""))

(defn ofsFormat
  "Serialises a list of entities into a valid OFN-S expression."
  [& args]
  (let [s (seq args)
        firstOperator (str "\"" (first s) "\"")
        arguments (interpose, "," (rest s))
        string (apply str arguments)
        brackets (str "[" firstOperator "," string "]")]
    brackets))

;this is used for lists because we don't interpret them as expressions
(defn ofsFormatNoBrackets
  "Serialises a list of entities into a valid OFN-S expression."
  [& args]
  (let [s (seq args)
        commas (interpose, "," s)
        string (apply str commas)]
    string))
