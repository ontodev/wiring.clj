(ns wiring.ldtab2ofn.spec
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]
            [clojure.spec.alpha :as spec]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                General OWL Types 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::map map?)

(spec/def ::individual string?)

;TODO: could we be more precise for class constructors 
;by enumerating all possiblities?
;Doing so would have the disadvantage of invalidating data input at the wrong place.
;I would like to now *which* class constructor is invalid and why.
;This is already handled by the current design because a class constructor will
;need to be translated and be validated by its corresponding spec.
(spec/def ::classExpression (spec/or :namedClass string?
                                  :classConstructor ::map))

(spec/def ::propertyExpression (spec/or :namedProperty string?
                                     :propertyConstructor ::map))

(spec/def ::rdf:type #{"owl:Restriction", "rdfs:Datatype", "owl:Class"})

(def cardinality-regex #"^[\d+]\^\^xsd:nonNegativeInteger$") 
(spec/def ::cardinality (spec/and string? #(re-matches cardinality-regex %)))

(spec/def ::predicateMap (spec/or :map ::map ;a predicate map is a map
                                  :value string?)) ;or a string as a base type

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Triple Encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::owl:onProperty ::propertyExpression)

(spec/def ::owl:onClass ::classExpression)

(spec/def ::owl:inverseOf ::propertyExpression)

(spec/def ::owl:someValuesFrom ::classExpression)

(spec/def ::owl:allValuesFrom ::classExpression)

(spec/def ::owl:hasValue ::individual)

;NOTE: one should not require specific values with spec
;I am making an exception here since the specified value is the ONLY allowed value
(spec/def ::owl:hasSelf #(= % "true^^xsd:boolean")) 

(spec/def ::owl:minCardinality ::cardinality) 
(spec/def ::owl:minQualifiedCardinality ::cardinality)

(spec/def ::owl:maxCardinality ::cardinality) 
(spec/def ::owl:maxQualifiedCardinality ::cardinality)

(spec/def ::owl:cardinality ::cardinality) 
(spec/def ::owl:qualifiedCardinality ::cardinality)

(spec/def ::owl:complementOf ::classExpression) 

(spec/def ::owl:intersectionOf ::map); "owl:intersectionOf": [{"object": {"rdf:first": ...  
(spec/def ::owl:unionOf ::map) 
(spec/def ::owl:oneOf ::map) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      RDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;I would like to introduce lists for class expressions
;however, 'rdf:first' and 'rdf:rest' are *not* only used for class expressions
;so, they have to be kept in this general form
(spec/def ::rdf:first (spec/or :map ::map
                               :end string?))

(spec/def ::rdf:rest (spec/or :map ::map
                              :end string?)) 

(spec/def ::list (spec/keys :req-un [::rdf:first ::rdf:rest]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Restrictions for Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(spec/def ::restriction (spec/keys :req-un [::owl:onProperty]
                                   :opt-un [::rdf:type]))

(spec/def ::existential (spec/merge ::restriction 
                                    (spec/keys :req-un [::owl:someValuesFrom])))

(spec/def ::universal (spec/merge ::restriction
                                  (spec/keys :req-un [::owl:allValuesFrom])))

(spec/def ::hasValue (spec/merge ::restriction
                                 (spec/keys :req-un [::owl:hasValue])))

(spec/def ::hasSelf (spec/merge ::restriction
                                (spec/keys :req-un [::owl:hasSelf])))

(spec/def ::minCardinality (spec/merge ::restriction
                                       (spec/keys :req-un [::owl:minCardinality])))

(spec/def ::minQualifiedCardinality (spec/merge ::restriction
                                                (spec/keys :req-un [::owl:minQualifiedCardinality ::owl:onClass])))

(spec/def ::maxCardinality (spec/merge ::restriction
                                       (spec/keys :req-un [::owl:maxCardinality])))

(spec/def ::maxQualifiedCardinality (spec/merge ::restriction
                                                (spec/keys :req-un [::owl:maxQualifiedCardinality ::owl:onClass])))

(spec/def ::exactCardinality (spec/merge ::restriction
                                         (spec/keys :req-un [::owl:cardinality])))

(spec/def ::exactQualifiedCardinality (spec/merge ::restriction
                                                  (spec/keys :req-un [::owl:qualifiedCardinality ::owl:onClass])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Propositional Connectives for Class Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(spec/def ::classIntersection (spec/keys :req-un [::owl:intersectionOf]
                                         :opt-un [::rdf:type]))

(spec/def ::classUnion (spec/keys :req-un [::owl:unionOf]
                                  :opt-un [::rdf:type]))

(spec/def ::oneOf (spec/keys :req-un [::owl:oneOf]
                             :opt-un [::rdf:type])) 

(spec/def ::classComplement (spec/keys :req-un [::owl:complementOf]

                                       :opt-un [::rdf:type]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Object Property Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(spec/def ::inverseOf (spec/keys :req-un [::owl:inverseOf]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Thick Triples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::subject (spec/or :atom string?))

(spec/def ::predicate (spec/or :atom string?))

(spec/def ::object (spec/or :atom string?
                               :constructor ::map)) 

(spec/def ::thickTriple (spec/keys :req-un [::subject ::predicate ::object]))
