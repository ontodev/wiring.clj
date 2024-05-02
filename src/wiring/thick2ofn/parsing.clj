(ns wiring.thick2ofn.parsing
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.spec :as owlspec]))

(defn handleNamespaces
  "Replace standard namespace IRI's with their name prefixes"
  [input]
  (let [owl (s/replace input #"http://www.w3.org/2002/07/owl#" "owl:")
        rdf (s/replace owl #"http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf:")
        rdfs (s/replace rdf #"http://www.w3.org/2000/01/rdf-schema#" "rdfs:")
        xsd (s/replace rdfs #"http://www.w3.org/2001/XMLSchema#" "xsd:")]
    xsd))

(defn parse
  "Parse JSON predicate map."
  [predicateMap]
  {:pre [(spec/valid? string? predicateMap)] ;a 'map' can be just a string, i.e. a normal object of a triple
   :post [(spec/valid? ::owlspec/map %)]}
  (let [removedObject (s/replace predicateMap #"\[\{\"object\":" "")
        removedClosingBracket (s/replace removedObject  #"\}\]" "")
        namespacePrefixes (handleNamespaces removedClosingBracket)
        predicates (cs/parse-string namespacePrefixes  true)]
    predicates))
