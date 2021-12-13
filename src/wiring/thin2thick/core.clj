(ns wiring.thin2thick.core
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as string]
            [wiring.thin2thick.annotation-handling :as post]
            [cheshire.core :as cs])
  (:gen-class))

;TODO tail recursion / iteration (not really necessary? performance gain is not significant)

(defn map-subject-2-thin-triples
  [thin-triples]
  """Given a set of thin triples,
    return a map from subjects to thin triples."""
  (group-by first thin-triples)) 

(defn is-blank-node?
  [node]
  (string/starts-with? node "_:"))

(defn is-rdf-type?
  [string]
  (or (= string "rdf:type")
      (= string "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>")))

(declare node-2-thick-map)

(defn map-on-hash-map-vals [f m]
    (zipmap (keys m) (map f (vals m)))) 

(defn triples-2-object
  [triples subject-2-thin-triples]
   (into [] (map #(hash-map :object (node-2-thick-map (nth % 2) subject-2-thin-triples)) triples))) 

(defn get-type
  [triples]
  (let [typing-triples (filter #(is-rdf-type? (second %)) triples)
        number-of-types (count typing-triples)]
    (cond
      (= number-of-types 0) "unknown"
      (= number-of-types 1) (nth (first typing-triples) 2)
      :else "ambiguous")))

;This works okay for
;-rdf:type owl:AllDisjointClasses
;-rdf:type owl:AllDisjointProperties .
;-rdf:type owl:AllDifferent
;-rdf:type owl:NegativePropertyAssertion ;
(defn encode-blank-nodes
  [triples]
  """Given a set of triples,
    identify root blank nodes and add triples of the form

    [wiring:blanknode:id type _:blankNode]

    where 'wiring:blanknode:id' is a newly generated subject,
    type is the rdf:type of the identified root _:blankNode,
    and _:blankNode is the root node. 

    For example given the triples:

       [_:B, obo:IAO_0010000, obo:050-003]
       [_:B, owl:annotatedTarget, 'target']
       [_:B, owl:annotatedProperty, obo:IAO_0000602]
       [_:B, owl:annotatedSource, obo:BFO_0000020]
       [_:B, rdf:type, owl:Axiom]

    the following triple would be added:

       [wiring:blanknode:1, rdf:type, _:B] 
    """
  (let [subject-to-triples (group-by first triples)
        subjects (set (map first triples))
        objects (set (map #(nth % 2) triples))
        root (s/difference subjects objects)
        blank-roots (filter is-blank-node? root)
        additions (map #(vector (str "wiring:blanknode:" (gensym))
                                (get-type (get subject-to-triples %))
                                %) blank-roots)] 
    (concat triples additions)))



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
        objects (map #(nth % 2) triples)
        object-blanknode (set (filter is-blank-node? objects))
        root (s/difference subjects object-blanknode)
        root-triples (filter #(contains? root (first %)) triples)]
    root-triples))


(defn sort-json
  [m]
  """Given a JSON value, return a lexicographically ordered representation"""
    (cond
      (map? m) (into (sorted-map) (map-on-hash-map-vals sort-json m));sort by key
      (coll? m) (into [] (map cs/parse-string (sort (map #(cs/generate-string (sort-json %)) m))))
      :else m))

(defn thin-2-thick
  ([triples]
   """Given a set of thin triples, return the corresponding set of thick triples."""
   (let [encoded-triples (encode-blank-nodes triples)
         s2t (map-subject-2-thin-triples encoded-triples)
         root (root-triples encoded-triples)
         thick (map #(thin-2-thick % s2t) root)
         sorted (map sort-json thick)]
     sorted))
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

;TODO compare maps so that they can be ordered
;note to self: you need this because you want to order thick triples
;so that you can match triples to their annotations quickly 
;(defn com
;  [m1 m2]
;  (let [s1 (:subject m1)
;        s2 (:subject m2)
;        p1 (:predicate m1)
;        p2 (:predicate m2) 
;        o1 (:predicate m1)
;        o2 (:predicate m2)
;        s-comp (compare s1 s2)
;        p-comp (compare p1 p2)
;        o-comp (compare o1 o2)]
;    (cond 
;      (not (= (compare s1 s2)

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
               ["ex:D", "ex:p", "ex:G"]])
  ;introduce dummy ;blank nodes;
 ;dont't translate root blank nodes?
  (def annotation [;["wiring:blanknode", "owl:AxiomAnnotatino", "_:B"],
                   ["obo:BFO_0000020", "obo:IAO_0000602", "asd"],
                   ["_:B", "obo:IAO_0010000", "obo:050-003"],
                   ["_:B", "owl:annotatedTarget", "asd"],
                   ["_:B", "owl:annotatedProperty", "obo:IAO_0000602"],
                   ["_:B", "owl:annotatedSource", "obo:BFO_0000020"],
                   ["_:B", "rdf:type", "owl:Axiom"]])
  (def disjointClasses [["_:B1", "owl:members", "_:B2"],
                        ["_:B1", "rdf:type", "owl:AllDisjointClasses"],
                        ["_:B2", "rdf:rest", "_:B3"],
                        ["_:B2", "rdf:first", "obo:BFO_0000142"],
                        ["_:B3", "rdf:rest", "_:B4"],
                        ["_:B3", "rdf:first", "obo:BFO_0000146"],
                        ["_:B4", "rdf:rest", "rdf:nil"],
                        ["_:B4", "rdf:first", "obo:BFO_0000147"]])

  (def merged [ 
                   ["obo:BFO_0000020", "obo:IAO_0000602", "asd"],
                   ["_:B", "obo:IAO_0010000", "obo:050-003"],
                   ["_:B", "owl:annotatedTarget", "asd"],
                   ["_:B", "owl:annotatedProperty", "obo:IAO_0000602"],
                   ["_:B", "owl:annotatedSource", "obo:BFO_0000020"],
                   ["_:B", "rdf:type", "owl:Axiom"],
                   ["_:B1", "owl:members", "_:B2"],
                   ["_:B1", "rdf:type", "owl:AllDisjointClasses"],
                   ["_:B2", "rdf:rest", "_:B3"],
                   ["_:B2", "rdf:first", "obo:BFO_0000142"],
                   ["_:B3", "rdf:rest", "_:B4"],
                   ["_:B3", "rdf:first", "obo:BFO_0000146"],
                   ["_:B4", "rdf:rest", "rdf:nil"],
                   ["_:B4", "rdf:first", "obo:BFO_0000147"]])

    ;ann = Annotation( Annotation( Annotation ( r:hasRole r:Curator ) a:author a:Seth_MacFarlane ) rdfs:label "Peter Griffin" )
    ;TANN(ann, a:Peter)
  (def recursive_annotation [ 
                              ["a:Peter" "rdfs:label", "Peter Griffin"],
                              ["_:x", "rdf:type", "owl:Annotation"],
                              ["_:x", "owl:annotatedSource", "a:Peter"],
                              ["_:x", "owl:annotatedProperty", "rdfs:label"],
                              ["_:x", "owl:annotatedTarget", "Peter Griffin"],
                              ["_:x", "a:author", "a:Seth_MacFarlane"],
                              ["_:x", "a:createdAt", "18.02.2021"],
                              ["_:y", "rdf:type", "owl:Annotation"],
                              ["_:y", "owl:annotatedSource", "_:x"],
                              ["_:y", "owl:annotatedProperty", "a:author"],
                              ["_:y", "owl:annotatedTarget", "a:Seth_MacFarlane"],
                              ["_:y", "r:hasRole", "r:Curator"]])
(def recursive_annotation-2 [ 
                              ["a:Peter" "rdfs:label", "Peter Griffin"],
                              ["_:x", "rdf:type", "owl:Annotation"],
                              ["_:x", "owl:annotatedSource", "a:Peter"],
                              ["_:x", "owl:annotatedProperty", "rdfs:label"],
                              ["_:x", "owl:annotatedTarget", "Peter Griffin"],
                              ["_:x", "a:author", "a:Seth_MacFarlane"],
                              ["_:x", "a:createdAt", "18.02.2021"],
                              ["_:y", "rdf:type", "owl:Annotation"],
                              ["_:y", "owl:annotatedSource", "_:x"],
                              ["_:y", "owl:annotatedProperty", "a:author"],
                              ["_:y", "owl:annotatedTarget", "a:Seth_MacFarlane"],
                              ["_:y", "r:hasRole", "r:Curator"], 
                              ["_:z", "rdf:type", "owl:Annotation"],
                              ["_:z", "owl:annotatedSource", "_:y"],
                              ["_:z", "owl:annotatedProperty", "r:hasRole"],
                              ["_:z", "owl:annotatedTarget", "r:Curator"],
                              ["_:z", "r:assingedBy", "r:Chris"] 
                              ])

  (println (thin-2-thick recursive_annotation))

  ;(def s2t (map-subject-2-thin-triples t))
  ;(run! println (map cs/generate-string (thin-2-thick annotation)))
  ;(println (thin-2-thick disjointClasses))
  (println "")
  (println (thin-2-thick annotation))
  (println "")
  (println (post/encode-raw-annotation-map-base (:object (second (thin-2-thick annotation)))))
  (println "")
  (println (sort-json (cs/parse-string (cs/generate-string (post/encode-raw-annotation-map (:object (second (thin-2-thick recursive_annotation))))))))
  (println "")
  (println (sort-json (cs/parse-string (cs/generate-string (post/encode-raw-annotation-map (:object (second (thin-2-thick recursive_annotation-2))))))))
  ;(println (post/encode-raw-annotation-map 
  ;(println "")
  ;(println (thin-2-thick merged))


  (println (str "wiring:" (gensym)))

  ;(run! println (map cs/generate-string (thin-2-thick disjointClasses)))
  ;(run! println (map cs/generate-string (thin-2-thick t)))
  ;(run! println (map #(thin-2-thick % s2t) (root-triples t)))
  ;(def ii (node-2-thick-map "_:B" s2t))
  ;(println ii)

  ;(println (cs/generate-string (sort-json ii)))
  ;(println (cs/generate-string (into (sorted-map) ii)))
  ;(println (cs/generate-string ii)) 
  )


