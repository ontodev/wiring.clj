(ns wiring.thin2thick.core
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as string]
            [cheshire.core :as cs])
  (:gen-class))

;TODO reification
;TODO special cases involving blank node generation
;TODO tail recursion / iteration (not really necessary? performance gain is not significant)

(defn map-subject-2-thin-triples
  [thin-triples]
  """Given a set of thin triples,
    return a map from subjects to thin triples."""
  (group-by first thin-triples)) 

(defn is-blank-node?
  [node]
  (string/starts-with? node "_:"))

(declare node-2-thick-map)

;TODO document this properly
;TODO refactor this

(defn map-on-hash-map-vals [f m]
    (zipmap (keys m) (map f (vals m)))) 

(defn triples-2-object
  [triples subject-2-thin-triples]
   (into [] (map #(hash-map :object (node-2-thick-map (nth % 2) subject-2-thin-triples)) triples))) 

;TODO: would this work for a "root blank node"? (no - all blank nodes get replaced with a predicate map)
(defn node-2-thick-map
  [node subject-2-thin-triples]
  """Given a node, i.e., a subject and a map from subjects to triples,
    returns a thick triple with 'node' as a subject"""
  (if (is-blank-node? node)
    (let [triples (get subject-2-thin-triples node)
          predicates (group-by second triples)]; create predicate map 
      (map-on-hash-map-vals #(triples-2-object % subject-2-thin-triples) predicates)) ;recurse on all predicate maps
    node))

(defn root-triples
  [triples]
  """Given a set of thin triples,
    return all triples that are at the root of a blank node dependency chain, i.e.
    triples s.t. its subject is not a blank node that occurs as an object in another triple."""
  (let [subjects (set (map first triples))
        objects (set (map #(nth % 2) triples))
        root (s/difference subjects objects)
        root-triples (filter #(contains? root (first %)) triples)]
    root-triples))

;TODO: impose an ordering
(defn thin-2-thick
  ([triples]
   """Given a set of thin triples, return the corresponding set of thick triples."""
   (let [s2t (map-subject-2-thin-triples triples)
         root (root-triples triples)
         thick (map #(thin-2-thick % s2t) root)
         json (map cs/generate-string thick)]
    json ))
  ([triple subject-2-thin-triples]
  """Given a thin triple t and a map from subjects to thin triples,
    return t as a thick triple."""
  (let [s (first triple)
        p (second triple)
        o (nth triple 2) 
        subject (node-2-thick-map s subject-2-thin-triples)
        predicate (node-2-thick-map p subject-2-thin-triples)
        object (node-2-thick-map o subject-2-thin-triples)]
    {:subject subject, :predicate predicate, :object object}))) 

;sort json recursively by its string representation
;TODO test this
(defn sort-json
  [s];expect this to be a string
  (let [m (cs/parse-string s)]
    (cond
      (map? m) (cs/generate-string (into (sorted-map) (map-on-hash-map-vals #(cs/parse-string (sort-json (cs/generate-string %))) m)));sort by key
      (coll? m) (cs/generate-string (into [] (map cs/parse-string (sort (map #(sort-json (cs/generate-string %)) m)))))
      :else (cs/generate-string m))))

(defn -main
  "Currently only used for manual testing."
  [& args]
  (def t [["ex:A", "rdf:subClassOf", "_:B"],
               ["_:B", "rdf:type", "owl:Restriction"],
               ["_:B", "owl:onProperty", "ex:p"],
               ["_:B", "owl:someValuesFrom", "_:C"],
               ["_:C", "rdf:type", "owl:Restriction"],
               ["_:C", "owl:onProperty", "ex:p"],
               ["_:C", "owl:onProperty", "ex:a"],
               ["_:C", "owl:onProperty", "_:E"],
               ["_:E", "owl:onProperty3", "F"],
               ["_:E", "owl:onProperty2", "E"],
               ["_:C", "owl:someValuesFrom", "D"],
               ["ex:B", "ex:d", "ex:C"],
               ["ex:D", "ex:p", "ex:F"],
               ["ex:D", "ex:p", "ex:G"]]
                 )
  (def s2t (map-subject-2-thin-triples t))
  ;(println s2t)
  ;(def ii (node-2-thick-map "ex:A" s2t))
  (def ii (node-2-thick-map "_:B" s2t))
  (println ii)
  ;(println (cs/generate-string ii))
  ;(println (cs/parse-string (cs/generate-string ii)))

  ;parse-string
  (println (sort-json (cs/generate-string ii)))
  ;(println (cs/generate-string (into (sorted-map) ii)))
  ;(println (cs/generate-string ii)) 
  ;(println (map #(thin-2-thick % s2t) (root-triples t)))


  ;(println t)
  ;(println "")
  ;(println (thin-2-thick t)) 
  )


