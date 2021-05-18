(ns wiring.util
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.spec :as owlspec]))

(defn typed? [predicates]
  (contains? predicates :rdf:type))

(defn getNumber [xsd]
  "Extract n from \"n\"^^xsd:nonNegativeInteger."
  (s/replace (first (s/split xsd #"\^")) #"\"" ""))

(defn ofsFormat
  "Serialises a list of entities into a valid OFN-S expression."
  [& args]
  (let [s (seq args)
        quotations (map #(str "\"" % "\"") s) 
        commas (interpose "," quotations)
        string (apply str commas)
        brackets (str "[" string "]")]
    brackets)) 
