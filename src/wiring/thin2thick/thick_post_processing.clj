(ns wiring.thin2thick.thick-post-processing
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as string]
            [cheshire.core :as cs])
  (:gen-class))

;TODO always work with CURIEs?

(defn is-annotation?
  [thick-triple]
  (let [predicate (:predicate thick-triple)]
    (or (= predicate "owl:Axiom")
        (= predicate "<http://www.w3.org/2002/07/owl#intersectionOf>"))))

;TODO: :object and (get coll object) should be used consistently
(defn get-annotated-triple
  [annotation]
  (let [predicate-map (:object annotation)
        subject (get (first (get predicate-map "owl:annotatedSource")) "object")
        predicate (get (first (get predicate-map "owl:annotatedProperty")) "object")
        object (get (first (get predicate-map "owl:annotatedTarget")) "object")]
    {:object object, :predicate predicate, :subject subject})) 

(defn get-annotations-for-assertions
  [thick-triples]
  """Given a set of thick triples,
    return anntations for asserted statements.""" 
  (let [triple-set (set thick-triples)
        annotations (filter is-annotation? thick-triples) 
        annotated-triples-contained (filter #(contains? triple-set (get-annotated-triple %)) annotations)]
    annotated-triples-contained))
