(ns wiring.thin2thick.annotation-handling
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as string]
            [cheshire.core :as cs])
  (:gen-class))

;TODO CURIEs
(defn is-annotation-map?
  [predicate-map]
  (and
    (contains? predicate-map "owl:annotatedSource")
    (contains? predicate-map "owl:annotatedProperty")
    (contains? predicate-map "owl:annotatedTarget"))) 

;NB this is something specific to wiring
(defn is-annotation-triple?
  [thick-triple]
  (let [predicate (:predicate thick-triple)]
    (= predicate "owl:Axiom")))

(defn is-owl-property?
  [property]
  (cond
    (= property "owl:annotatedSource") true
    (= property "owl:annotatedProperty") true
    (= property "owl:annotatedTarget") true
    (= property "rdf:type") true
    (= property "annotation") true ;NB this is something specific to thick triples
    :else false))

(defn update-annotation-map
  [annotation-map previous-annotation]
  (let [annotated-property (get previous-annotation "owl:annotatedProperty")
        annotated-object (get previous-annotation "owl:annotatedTarget")]
    (update annotation-map annotated-property #(into [] (map 
                                                          (fn [x] 
                                                            (if (= (:object x) annotated-object)
                                                              (assoc x :annotation (:annotation previous-annotation))
                                                              x)) 
                                                          %))))) 

(defn encode-raw-annotation-map-base
  [predicate-map previous-annotation]
  (let [subject (:object (first (get predicate-map "owl:annotatedSource")))
        predicate (:object (first (get predicate-map "owl:annotatedProperty")))
        object (:object (first (get predicate-map "owl:annotatedTarget"))) 

        annotation-properties (filter #(not (is-owl-property? %)) (keys predicate-map))
        annotation-objects (map #(get predicate-map %) annotation-properties) 
        annotation-map (zipmap annotation-properties annotation-objects)] 

    (if (not-empty previous-annotation)
      {:object object,
       :predicate predicate,
       :subject subject,
       :annotation (update-annotation-map annotation-map previous-annotation)} 
      {:object object,
       :predicate predicate,
       :subject subject,
       :annotation annotation-map})))

(declare encode-raw-annotation-map)

(defn encode-raw-annotation-map-recursion
  [predicate-map previous-annotation]
  (let [subject (:object (first (get predicate-map "owl:annotatedSource")))
        predicate (:object (first (get predicate-map "owl:annotatedProperty")))
        object (:object (first (get predicate-map "owl:annotatedTarget")))

        annotation-properties (filter #(not (is-owl-property? %)) (keys predicate-map))
        annotation-objects (map #(get predicate-map %) annotation-properties) 
        annotation-map (zipmap annotation-properties annotation-objects)
        updated-annotation (update-annotation-map annotation-map previous-annotation)] 

    (if (empty? previous-annotation)
      (encode-raw-annotation-map subject {:annotation annotation-map
                                          "owl:annotatedProperty" predicate
                                          "owl:annotatedTarget" object}) 
      (encode-raw-annotation-map subject
                                 {:annotation updated-annotation
                                  "owl:annotatedProperty" predicate
                                  "owl:annotatedTarget" object})
      )))


(defn encode-raw-annotation-map
  ([predicate-map]
   (encode-raw-annotation-map predicate-map {}))
  ([predicate-map previous-annotation]
  (let [subject (:object (first (get predicate-map "owl:annotatedSource")))]
    (if (map? subject) 
      (encode-raw-annotation-map-recursion predicate-map previous-annotation)
      (encode-raw-annotation-map-base predicate-map previous-annotation)))))


(defn get-annotated-triple
  [annotation]
  (let [predicate-map (:object annotation)
        subject (:object (first (get predicate-map "owl:annotatedSource")))
        predicate (:object (first (get predicate-map "owl:annotatedProperty")))
        object (:object (first (get predicate-map "owl:annotatedTarget")))]
    {:object object, :predicate predicate, :subject subject})) 

(defn get-annotations-for-assertions
  [thick-triples]
  """Given a set of thick triples,
    return anntations for asserted statements.""" 
  (let [triple-set (set thick-triples)
        annotations (filter is-annotation-triple? thick-triples) 
        annotated-triples-contained (filter #(contains? triple-set (get-annotated-triple %)) annotations)]
    annotated-triples-contained))

