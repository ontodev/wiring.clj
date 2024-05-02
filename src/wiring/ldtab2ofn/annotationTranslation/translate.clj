(ns wiring.ldtab2ofn.annotationTranslation.translate
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [cheshire.core :as cs]
            [clojure.edn :as edn]
            [wiring.ldtab2ofn.spec :as owlspec]))

;TODO data validation for return values
(declare translate)

;use Annotation (for OWL stuff) AND AnnotationList (for RDF) 
(defn translate-annotation
  [predicateMap]
  (println predicateMap))

(defn translate-reification
  [predicateMap]
  (println predicateMap))

(defn translate-owl
  [predicateMap]
  (let [[k v] (first (seq predicateMap))]
    (println v)))

(defn translate-rdf
  [predicateMap]
  (println predicateMap))

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (let [number (count (seq predicateMap))]
    (if (= 1 number) ;only one property - is not enough for owl - also need only one value in list
      (translate-owl predicateMap)
      (translate-rdf predicateMap))))

 ;       anno (:annotation predicateMap);TODO: need to reach down..
 ;       metatype (:meta anno)];
 ;   (case metatype
 ;     "owl:Annotation" (translate-annotation predicateMap)
 ;     "owl:Axiom" (translate-annotation predicateMap)
 ;     "rdf:Statement" ;rdf:Reification?
 ;   )))
