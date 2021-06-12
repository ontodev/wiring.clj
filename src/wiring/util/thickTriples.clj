(ns wiring.util.thickTriples
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [wiring.thick2ofn.parsing :as p]
            [clojure.spec.alpha :as spec]))

;TODO handle ABox vs TBox

(defn triple?
  "Checks whether a map is a triple." 
  [input]
  (and
    (map? input)
    (contains? input :subject)
    (contains? input :predicate)
    (contains? input :object)))

(defn thickTriple?
  "Checks whether a triple is a thick triple." 
  [triple]
  (and (triple? triple)
       (map? (:object triple))
       (not (:predicate "rdf:type"))));TODO: we can have 'thick triples of the form'
;{:subject obo:OBI_0000958, :predicate rdf:type, :object {:owl:onProperty obo:IAO_0000219, :rdf:type owl:Restriction, :owl:someValuesFrom obo:PATO_0002203}} - this is an ABox axiom. But we don't translate them yet. So the translation mechanism will return the empty string for this.
;so for the time being, I will just filter them out here


(defn thinTriple?
  "Checks whether a triple is a thin triple." 
  [triple]
  (and (triple? triple)
       (not (thickTriple? triple))))

(defn rdfType?
  "Checks whether an entity is a standard rdf:type." 
  [t]
  (case t
    "owl:Class" true
    "owl:NamedIndividual" true
    "owl:ObjectProperty" true
    "owl:Restriction" true
    "owl:AnnotationProperty" true
    ;TODO
    false))

(defn labelingTriple?
  "Checks whether a triple contains information about an entity's rdfs:label"
  [triple]
  (and (thinTriple? triple)
       (= (:predicate triple) "rdfs:label")))

(defn typingTriple?
  "Checks whether a triple contains information about an entity's rdf:type."
  [triple]
  (= (:predicate triple) "rdf:type"))

(defn rdfTypingTriple?
  "Checks whether a triple contains information about a standard RDF type"
  [triple]
  (and (typingTriple? triple)
       (rdfType? (:object triple)))) 

(defn updateSubject2info
  "Adds an element to a map."
  [subject2types typingTriple]
  (let [subject (:subject typingTriple)
        info (:object typingTriple)]
    (if (contains? subject2types subject)
      (assoc subject2types subject (conj (get subject2types subject) info))
      (assoc subject2types subject #{info}))))
