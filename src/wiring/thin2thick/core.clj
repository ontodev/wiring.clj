(ns wiring.thin2thick.core
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as string]
            [wiring.thin2thick.annotation-handling :as ann]
            [cheshire.core :as cs])
  (:gen-class))


(defn is-blank-node?
  "Given a node, i.e., an RDF subject, predicate, or object,
   return true if the node is a blank node."
  [node]
  (string/starts-with? node "_:"))

(defn is-rdf-type?
  [string]
  (or (= string "rdf:type")
      (= string "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>");TODO this will be unnecessary
      (= string "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")))

(defn is-rdf-literal?
  [string]
  (when (string? string)
    (re-matches #"^\".*\".*$" string))) 

;NB: quotation marks will be part of the string representation 
;so (get-literal-string "\"example\""@en) will return
;"\"example\"" and not "example"
;this is done to preserve type information
(defn get-literal-string
  [string]
 (second (re-matches #"^(\".*\").*$" string)))

(defn has-language-tag?
  [literal]
  (re-matches #"^\"(.*)\"(@.*)$" literal))

(defn has-datatype?
  [literal]
  (re-matches #"^\"(.*)\"(\^\^.*)$" literal))


(declare node-2-thick-map)

(defn map-on-hash-map-vals
  "Given a hashmap m and a function f, 
  apply f to all values of m.
  Example:
  Given m = {:a 1, :b 2}  and f = (fn [x] (inc x)),
  then (map-on-hash-map-vals f m) = {:a 2, :b 3}"
  [f m]
  (zipmap (keys m) (map f (vals m)))) 


(defn get-type
  [triples]
  (let [typing-triples (filter #(is-rdf-type? (second %)) triples)
        number-of-types (count typing-triples)]
    (cond
      (= number-of-types 0) "unknown"
      (= number-of-types 1) (nth (first typing-triples) 2)
      :else "ambiguous")))

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

    Explanation:
    We collapse blank nodes into JSON maps.
    However, for root blank nodes, this yields a JSON map that is not part of a triple.
    So, we artificially create these triples by introducing 'dummy blank nodes' (that are not treated as blank nodes by the implementation).
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

(defn encode-object
  "Given a triple t = [s p o] and a map from subject nodes to its triples,
  returns predicate map for the o" 
  [triple subject-2-thin-triples]
  (hash-map :object (node-2-thick-map (nth triple 2) subject-2-thin-triples)))

(defn node-2-thick-map
  "Given a node and a map from subject nodes to its triples,
    returns a predicate map if its a blank node
    and itself otherwise" 
  [node subject-2-thin-triples]
  (if (is-blank-node? node)
    (let [triples (get subject-2-thin-triples node)
          predicates (group-by second triples)]; create predicate map 
      (map-on-hash-map-vals ;encode objects recursively
        #(into [] (map (fn [x] (encode-object x subject-2-thin-triples)) %))
        predicates)) 
    node)) 

(defn root-triples
  "Given a set of thin triples,
    return all triples that are at the root of a blank node dependency chain, i.e.,
    a triple s.t. its subject is not a blank node that
    occurs as an object in another triple."
  [triples]
  (let [subjects (set (map first triples))
        objects (map #(nth % 2) triples)
        object-blanknode (set (filter is-blank-node? objects))
        root (s/difference subjects object-blanknode)
        root-triples (filter #(contains? root (first %)) triples)]
    root-triples))

;NB: sorting transfoms keywords to strings 
(defn sort-json
  "Given a JSON value, return a lexicographically ordered representation."
  [m]
    (cond
      (map? m) (into (sorted-map) (map-on-hash-map-vals sort-json m)) ;sort by key
      (coll? m) (into [] (map cs/parse-string ;sort by string comparison
                              (sort (map #(cs/generate-string (sort-json %))
                                         m))))
      :else m))

(defn map-subject-2-thin-triples
  "Given a set of thin triples,
    return a map from subjects to thin triples."
  [thin-triples]
  (group-by first thin-triples)) 

(defn thin-2-thick-raw
  ([triples]
   """Given a set of thin triples, return the corresponding set of (raw) thick triples."""
   (let [blank-node-encoding (encode-blank-nodes triples)
         subject-2-thin-triples (map-subject-2-thin-triples blank-node-encoding)
         root-triples (root-triples blank-node-encoding)
         thick-triples (map #(thin-2-thick-raw % subject-2-thin-triples) root-triples)]
     thick-triples))
  ([triple subject-2-thin-triples]
  """Given a thin triple t and a map from subjects to thin triples,
    return t as a (raw) thick triple."""
  (let [s (first triple)
        p (second triple)
        o (nth triple 2) 
        subject (node-2-thick-map s subject-2-thin-triples)
        predicate (node-2-thick-map p subject-2-thin-triples)
        object (node-2-thick-map o subject-2-thin-triples)]
    {:subject subject, :predicate predicate, :object object}))) 


(declare encode-literals)

(defn encode-literal
  "Encode a single literal as described by 'encode-literals'"
  [predicate-map] 
  (let [object (:object predicate-map)
        literal-value (get-literal-string object)
        language-tag (has-language-tag? object)
        datatype (has-datatype? object)
        has-thick-datatype (or language-tag datatype)]
    (if has-thick-datatype
      (assoc predicate-map :object literal-value
             :datatype (nth has-thick-datatype 2))
      (assoc predicate-map :datatype "_plain"))))

(defn handle-object
  "Given a predicate map, update its JSON structure by
  (1) encoding an RDF literal if it occurs as an :object,  
  (2) recurse if :object is an JSON object or array 
  (3) return the predicate-map otherwise."
  [predicate-map]
    (let [object (:object predicate-map)]
      (cond (is-rdf-literal? object) (encode-literal predicate-map) 
            ;recurse
            (map? object) (update predicate-map :object encode-literals) 
            (coll? object) (update predicate-map :object #(map encode-literals %))
            ;base case
            :else predicate-map)))

;NB thick-triple has to be passed around instead of (:object thick-triple)
;because we need to modify the map 'thick-triple' and not just (:object thick-triple)
(defn encode-literals
  "Given a thick-triple,
  search for RDF literals that appear as :object in the thick-triple,
  extract their datatypes and language tags (if present), and
  encode them as :datatype in the corresponding predicate map.
  Example:
  {:subject a:Peter, :predicate rdfs:label, :object \"Peter Griffin\"@en}
  will be encoded as 
  {:subject a:Peter, :predicate rdfs:label, :object \"Peter Griffin\" :datatype @en}."
  [thick-triple]
  (if (map? thick-triple)
    ;predicate maps are only changed in case an :object contains an RDF literal
    ;otherwise, the algorithm only needs to traverse the whole JSON structure
    (let [object-handled (handle-object thick-triple)
          rest-handled (map-on-hash-map-vals #(cond ;JSON traversal
                                                (map? %) (encode-literals %)
                                                (coll? %) (into [] (map encode-literals %))
                                                :else %)
                                             (dissoc thick-triple :object))]
      (if (:object object-handled)
        (let [encoding (assoc rest-handled :object (:object object-handled))]
          (if (contains? object-handled :datatype)
            (assoc encoding :datatype (:datatype object-handled))
            encoding))
        rest-handled))
    thick-triple)) 

(defn thin-2-thick
  [triples]
  (let [raw-thick-triples (thin-2-thick-raw triples)
        ;TODO RDF reification (= (:predicate %) "rdf:statement")
        thick-triples (map #(if (or (= (:predicate %) "owl:Annotation")
                                    (= (:predicate %) "owl:Axiom")
                                    (= (:predicate %) "rdf:Statement"))
                              (ann/encode-raw-annotation-map (:object %)) 
                              %) raw-thick-triples)
        thick-triples (map encode-literals thick-triples)
        sorted (map sort-json thick-triples)
        normalised (map #(cs/parse-string (cs/generate-string %)) sorted)];TODO: stringify keys - this is a (probably an inefficient?) workaround 
    normalised))

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
                   ["obo:BFO_0000020", "obo:IAO_0000602", "\"asd\""],
                   ["_:B", "obo:IAO_0010000", "obo:050-003"],
                   ["_:B", "owl:annotatedTarget", "\"asd\""],
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
                              ["a:Peter" "rdfs:label", "\"Peter Griffin\"@en"],
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
                              ["_:z", "r:assingedBy", "\"r:Chris\"@en"] 
                              ])
(def reification [;["wiring:blanknode", "owl:AxiomAnnotatino", "_:B"], 
                   ["_:B", "obo:IAO_0010000", "obo:050-003"],
                   ["_:B", "rdf:object", "\"asd\""],
                   ["_:B", "rdf:predicate", "obo:IAO_0000602"],
                   ["_:B", "rdf:subject", "obo:BFO_0000020"],
                   ["_:B", "rdf:type", "rdf:Statement"]])

  (println (thin-2-thick-raw recursive_annotation-2)) ;gets raw thick triple
  (println "")
  (println (thin-2-thick recursive_annotation-2)) ;gets raw thick triple
  (println "")
  (println (thin-2-thick-raw annotation)) ;gets raw thick triple
  (println "")
  (println (thin-2-thick annotation)) ;gets raw thick triple
  (println "")
  (println (thin-2-thick reification)) ;gets raw thick triple
  (println "")
  ;(println (sort-json (cs/parse-string (cs/generate-string (ann/encode-raw-annotation-map (:object (second (thin-2-thick-raw recursive_annotation-2)))))))) 
)


