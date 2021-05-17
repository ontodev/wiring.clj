(ns wiring.rdf2ofs
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]
            [clojure.spec.alpha :as spec]
            [wiring.spec :as owlspec]))

(declare translate) ;recursive parsing (not tail recursive)  

(defn getNumber [xsd]
  "Extract n from \"n\"^^xsd:nonNegativeInteger."
  (s/replace (first (s/split xsd #"\^")) #"\"" ""))

(defn ofsFormat
  "Serialises a list of entities into a valid OFN-S expression."
  [& args]
  (let [s (seq args)
        quotations (map #(str "\"" % "\"") s) 
        commas (interpose "," quotations)
        string (apply str commas)
        brackets (str "[" string "]")]
    brackets)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   OWL Object Properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateInverseOf
  "Translate the inverse of a property"  
  [predicates]
  (let [property (:owl:inverseOf predicates)]
    (ofsFormat "ObjectInverseOf" property))) 

(defn translateExistentialRestriction
  "Translate an existential restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/existential predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (translate (:owl:onProperty predicates))
        filler (translate (:owl:someValuesFrom predicates))]
    (ofsFormat "ObjectSomeValuesFrom" onProperty filler)))

(defn translateUniversalRestriction
  "Translate a universal restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/universal predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (translate (:owl:onProperty predicates))
        filler (translate (:owl:allValuesFrom predicates))]
    (ofsFormat "ObjectAllValuesFrom" onProperty filler)))

(defn translateHasValueRestriction
  "Translate hasValue restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/hasValue predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (translate (:owl:onProperty predicates))
        filler (:owl:hasValue predicates)];individual
    (ofsFormat "ObjectHasValue" onProperty filler)))

(defn translateHasSelfRestriction
  "Translate hasSelf restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/hasSelf predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (translate (:owl:onProperty predicates))]
    (ofsFormat "ObjectHasSelf" onProperty)))

(defn translateMinCardinalityRestriction
  "Translate minimum cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/minCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (translate (:owl:onProperty predicates))
        cardinality (getNumber (:owl:minCardinality predicates))];
    (ofsFormat "ObjectMinCardinality" cardinality onProperty)))

(defn translateMinQualifiedCardinalityRestriction
  "Translate minimum qualified cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/minQualifiedCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (translate (:owl:onProperty predicates))
        cardinality (getNumber (:owl:minQualifiedCardinality predicates))
        filler (translate (:owl:onClass predicates))];
    (ofsFormat "ObjectMinCardinality" cardinality onProperty filler)))

(defn translateMaxCardinalityRestriction
  "Translate maximum cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/maxCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (translate (:owl:onProperty predicates))
        cardinality (getNumber (:owl:maxCardinality predicates))];
    (ofsFormat "ObjectMaxCardinality" cardinality onProperty)))

(defn translateMaxQualifiedCardinalityRestriction
  "Translate maximum qualified cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/maxQualifiedCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (translate (:owl:onProperty predicates))
        cardinality (getNumber (:owl:maxQualifiedCardinality predicates))
        filler (translate (:owl:onClass predicates))];
    (ofsFormat "ObjectMaxCardinality" cardinality onProperty filler)))

(defn translateExactCardinalityRestriction
  "Translate exact cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/exactCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (translate (:owl:onProperty predicates))
        cardinality (getNumber (:owl:cardinality predicates))];
    (ofsFormat "ObjectExactCardinality" cardinality onProperty)))

(defn translateExactQualifiedCardinalityRestriction
  "Translate exact qualified cardinality restriction."
  [predicates]
  {:pre [(spec/valid? ::owlspec/exactQualifiedCardinality predicates)]
   :post [(spec/valid? string? %)]}
  (let [onProperty (translate (:owl:onProperty predicates))
        cardinality (getNumber (:owl:qualifiedCardinality predicates))
        filler (translate (:owl:onClass predicates))];
    (ofsFormat "ObjectExactCardinality" cardinality onProperty filler)))

(defn translateRestriction [predicates]
  (cond
    (contains? predicates :owl:someValuesFrom) (translateExistentialRestriction predicates)
    (contains? predicates :owl:allValuesFrom) (translateUniversalRestriction predicates)
    (contains? predicates :owl:hasValue) (translateHasValueRestriction predicates)
    (contains? predicates :owl:hasSelf) (translateHasSelfRestriction predicates)
    (contains? predicates :owl:minCardinality) (translateMinCardinalityRestriction predicates)
    (contains? predicates :owl:minQualifiedCardinality) (translateMinQualifiedCardinalityRestriction predicates)
    (contains? predicates :owl:maxCardinality) (translateMaxCardinalityRestriction predicates)
    (contains? predicates :owl:maxQualifiedCardinality) (translateMaxQualifiedCardinalityRestriction predicates)
    (contains? predicates :owl:cardinality) (translateMaxCardinalityRestriction predicates)
    (contains? predicates :owl:qualifiedCardinality) (translateMaxQualifiedCardinalityRestriction predicates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      RDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateList
  "Translate an RDF list."
  [predicates]
  {:pre [(spec/valid? ::owlspec/list predicates)]
   :post [(spec/valid? string? %)]}
  (loop [in predicates
         out []]
    (if (= in "rdf:nil")
      (apply ofsFormat out)
      (recur (:rdf:rest in)
             (conj out (translate (:rdf:first in)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   OWL classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateIntersection
  "Translate an intersection."
  [predicates]
  {:pre [(spec/valid? ::owlspec/classIntersection predicates)]
   :post [(spec/valid? string? %)]}
  (let [arguments (translate (:owl:intersectionOf predicates))]
    (ofsFormat "ObjectIntersectionOf" arguments)))

(defn translateUnion
  "Translate a union."
  [predicates]
  {:pre [(spec/valid? ::owlspec/classUnion predicates)]
   :post [(spec/valid? string? %)]}
  (let [arguments (translate (:owl:unionOf predicates))]
    (ofsFormat "ObjectUnionOf" arguments)))

(defn translateOneOf
  "Translate a oneOf."
  [predicates]
  {:pre [(spec/valid? ::owlspec/oneOf predicates)]
   :post [(spec/valid? string? %)]}
  (let [arguments (translateList (:owl:oneOf predicates))];TODO: no translation for individuals yet
    (ofsFormat "ObjectOneOf" arguments)))

(defn translateComplement
  "Translate complement."
  [predicates]
  {:pre [(spec/valid? ::owlspec/classComplement predicates)]
   :post [(spec/valid? string? %)]}
  (let [argument (translate (:owl:complementOf predicates))]
    (ofsFormat "ObjectComplementOf" argument)))

(defn translateClass [predicates]
  "Translate OWL class expressions with an rdf:type."
  (cond
    (contains? predicates :owl:intersectionOf) (translateIntersection predicates)
    (contains? predicates :owl:unionOf) (translateUnion predicates)
    (contains? predicates :owl:complementOf) (translateComplement predicates)))

(defn parse
  "Parse JSON predicate map."
  [predicateMap]
  {:pre [(spec/valid? string? predicateMap)] ;a 'map' can be just a string, i.e. a normal object of a triple
   :post [(spec/valid? ::owlspec/map %)]}
  (let [removedObject (s/replace predicateMap #"\[\{\"object\":" "")
        removedClosingBracket (s/replace removedObject  #"\}\]" "")
        predicates (cs/parse-string removedClosingBracket true)]
    predicates))

(defn typed? [predicates]
  (contains? predicates :rdf:type))

(defn translateUntyped [predicates]
  "Translate expressions without rdf:type information."
  (let [restriction (translateRestriction predicates) ;returns nil if no translation is performed
        clas (translateClass predicates) ;returns nil if no translation is performed
        lis (contains? predicates :rdf:first)
        inverseProperty (contains? predicates :owl:inverseOf)]

    (cond
      restriction restriction
      clas clas
      lis (translateList predicates)
      inverseProperty (translateInverseOf predicates) ; this is the only property constructor
      :else "")))

(defn translateTyped [predicates]
  "Translate expressions with an rdf:type."
  (let [entrypoint (:rdf:type predicates)]
    (case entrypoint
      "owl:Restriction" (translateRestriction predicates)
      "owl:Class" (translateClass predicates)
      ;TODO
      ;rdfs:Datatype
      ;owl:AllDisjointProperties
      ;owl:allDifferent
      ;owl:NegativePropertyAssertion
      ;owl:FunctionalProperty..
      ;entrypoint ;default case for 'A rdf:type B'
      )))

(defn translate
  "Translate predicate map to OFS."
  [predicateMap]
  (cond
    (string? predicateMap) predicateMap ;base case
    (typed? predicateMap) (translateTyped predicateMap)
    :else (translateUntyped predicateMap)))
