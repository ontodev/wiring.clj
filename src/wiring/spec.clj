(ns wiring.spec
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]
            [clojure.spec.alpha :as spec]))

(spec/def ::map map?)

(spec/def ::predicateMap (spec/or :map ::map ;a predicate map is a map
                                  :value string?)) ;or a string as a base type

(spec/def ::owl:onProperty  (spec/or :namedProperty string?
                                     :propertyConstructor ::map))

(spec/def ::owl:onClass  (spec/or :namedClass string?
                                  :classConstructor ::map))

(spec/def ::owl:someValuesFrom (spec/or :namedClass string?
                                        :classConstructor ::map))

(spec/def ::owl:allValuesFrom (spec/or :namedClass string?
                                       :classConstructor ::map))

(spec/def ::owl:hasValue (spec/or :individual string?))

(spec/def ::owl:hasSelf (spec/or :boolean string?)) ;this is a 'boolean' but encoded as "true"^^xsd:boolean (TODO write a predicate for this particular string)

(spec/def ::owl:minCardinality (spec/or :cardinality string?)) ;this is a number but encoded as '"n"^^xsd:nonNegativeInteger.

(spec/def ::owl:minQualifiedCardinality (spec/or :cardinality string?)) ;this is a number but encoded as '"n"^^xsd:nonNegativeInteger.

(spec/def ::owl:maxCardinality (spec/or :cardinality string?)) ;this is a number but encoded as '"n"^^xsd:nonNegativeInteger.

(spec/def ::owl:maxQualifiedCardinality (spec/or :cardinality string?)) ;this is a number but encoded as '"n"^^xsd:nonNegativeInteger.

(spec/def ::owl:exactCardinality (spec/or :cardinality string?)) ;this is a number but encoded as '"n"^^xsd:nonNegativeInteger.

(spec/def ::owl:exactQualifiedCardinality (spec/or :cardinality string?)) ;this is a number but encoded as '"n"^^xsd:nonNegativeInteger.

(spec/def ::rdf:type #{"owl:Restriction", "rdfs:Datatype", "owl:Class"})

(spec/def ::rdf:first string?)

(spec/def ::rdf:rest ::map) ;;list

(spec/def ::owl:intersectionOf ::map); and this map needs to be a list

(spec/def ::owl:unionOf ::map); and this map needs to be a list

(spec/def ::owl:oneOf ::map); and this map needs to be a list

(spec/def ::owl:complementOf ::map)

(spec/def ::existential (spec/keys :req-un [::owl:onProperty ::owl:someValuesFrom]
                                   :opt-un [::rdf:type]))

(spec/def ::universal (spec/keys :req-un [::owl:onProperty ::owl:allValuesFrom]
                                 :opt-un [::rdf:type]))

(spec/def ::hasValue (spec/keys :req-un [::owl:onProperty ::owl:hasValue]
                                :opt-un [::rdf:type]))

(spec/def ::hasSelf (spec/keys :req-un [::owl:onProperty ::owl:hasSelf]
                               :opt-un [::rdf:type]))

(spec/def ::minCardinality (spec/keys :req-un [::owl:onProperty ::owl:minCardinality]
                                      :opt-un [::rdf:type]))

(spec/def ::minQualifiedCardinality (spec/keys :req-un [::owl:onProperty ::owl:minQualifiedCardinality ::owl:onClass]
                                               :opt-un [::rdf:type]))

(spec/def ::maxCardinality (spec/keys :req-un [::owl:onProperty ::owl:maxCardinality]
                                      :opt-un [::rdf:type]))

(spec/def ::maxQualifiedCardinality (spec/keys :req-un [::owl:onProperty ::owl:maxQualifiedCardinality ::owl:onClass]
                                               :opt-un [::rdf:type]))

(spec/def ::exactCardinality (spec/keys :req-un [::owl:onProperty ::owl:exactCardinality]
                                        :opt-un [::rdf:type]))

(spec/def ::exactQualifiedCardinality (spec/keys :req-un [::owl:onProperty ::owl:exactQualifiedCardinality ::owl:onClass]
                                                 :opt-un [::rdf:type]))

(spec/def ::restriction (spec/or :existential ::existential
                                 :universal ::universal))

(spec/def ::list (spec/keys :req-un [::rdf:first ::rdf:rest]))

(spec/def ::classIntersection (spec/keys :req-un [::owl:intersectionOf]
                                         :opt-un [::rdf:type]))

(spec/def ::classUnion (spec/keys :req-un [::owl:unionOf]
                                  :opt-un [::rdf:type]))

(spec/def ::oneOf (spec/keys :req-un [::owl:oneOf]
                             :opt-un [::rdf:type]))

(spec/def ::classComplement (spec/keys :req-un [::owl:complementOf]
                                       :opt-un [::rdf:type]))

(defn tests [predicateMap]
  (let [removedObject (s/replace predicateMap #"\[\{\"object\":" "")
        removedClosingBracket (s/replace removedObject  #"\}\]" "")
        predicates (cs/parse-string removedClosingBracket true)]

    (println (spec/valid? ::restriction predicates))))

