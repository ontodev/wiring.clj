(ns wiring.labelling.labelHandling
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [wiring.util.thickTriples :as thickTriples]
            [wiring.util.ofnExpressions :as ofnExpressions]))

(declare translate)

(defn render 
  "Rendering of a label L either as L or as 'L' in case L includes white spaces."
  [label]
  (let [hasBlank (some #(s/blank? (str %)) (set label))]
    (if hasBlank
      (str "'" label "'")
      label)))

(defn labelingTriple?
  "Check whether an OFN-S expression contains a label"
  [ofn]
   (and (= (first ofn) "ThinTriple")
        (= (nth ofn 2) "rdfs:label"))) 

(defn id
  "Identity function for OFN-S expressions."
  [ofn subject2label]
  (if (ofnExpressions/namedEntity? ofn)
    (if (contains? subject2label ofn) 
      (render (first (get subject2label ofn)))
      (render ofn))
    (let [[operator & arguments] ofn
           args (map #(translate % subject2label) arguments)
           op (vec (cons operator args))]
      op))) 

(defn translate
  "Translate (abstract) OFN-S expression to an OFN-S expression"
  [ofn subject2label]
  (if (labelingTriple? ofn)
    ofn
    (id ofn subject2label)))

(defn translateSet
  "Translate a set of (abstract) OFN-S expressions"
  [ofns subject2label]
  (map #(translate % subject2label) ofns)) 




(defn updateSubject2info
  "Adds an element to a map."
  [subject2labels labelExpression]
  (let [subject (second labelExpression)
        info (nth labelExpression 3)]
    (if (contains? subject2labels subject)
      (assoc subject2labels subject (conj (get subject2labels subject) info))
      (assoc subject2labels subject #{info})))) 

(defn extractLabelling
  "Takes a colelction of OFN-S expressions and returns a map for labels"
  [ofns]
  (let [labels (filter labelingTriple? ofns)
        subject2labels (reduce updateSubject2info (hash-map) labels)]
  subject2labels))


(defn inlineLabels
  "Takes an ontology represented by OFN-S expressions and
  returns the ontology with labels substituted in for IRI's"
  [ontology]
  (let [subject2labels (extractLabelling ontology)
        labelling (translateSet ontology subject2labels)]
    labelling))


