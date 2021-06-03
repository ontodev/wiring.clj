(ns wiring.ofn2thick.classTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [wiring.thick2ofn.spec :as spec])
  (:gen-class))

;TODO data validation
(declare translate) 

(defn translateList
  "Translate class expressions into an RDF list"
  [expressions]
  (loop [in (reverse expressions);constructing list from last element to first
    out "\"rdf:nill\""]
    (if (empty? in)
      out
      (recur (rest in)
        (str "{\"rdf:first\": " (translate (first in)) ", " "\"rdf:rest\": [{\"object\": " out "}]}")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Restrictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateObjectSomeValuesFrom
  "Translate a ObjectSomeValuesFrom expression"
  [ofn]
  (let [[op property filler] ofn
        someValues "{\"owl:someValuesFrom\": "
        filler (str someValues "[{\"object\": \"" filler "\"}], ") 
        property (str filler "\"owl:onProperty\": [{\"object\": \"" property "\"}], ")
        rdfType (str property "\"rdf:type\": [{\"object\": \"owl:Restriction\"}]")
        closing (str rdfType "}")]
    closing))

(defn translateObjectAllValuesFrom
  "Translate a ObjectAllValuesFrom expression"
  [ofn]
  (let [[op property filler] ofn
        allValues "{\"owl:allValuesFrom\": "
        filler (str allValues "[{\"object\": \"" filler "\"}], ") 
        property (str filler "\"owl:onProperty\": [{\"object\": \"" property "\"}], ")
        rdfType (str property "\"rdf:type\": [{\"object\": \"owl:Restriction\"}]")
        closing (str rdfType "}")]
    closing))

(defn translateObjectHasValue
  "Translate a ObjectHasValue expression"
  [ofn]
  (let [[op property filler] ofn
        hasValue "{\"owl:hasValue\": "
        filler (str hasValue "[{\"object\": \"" filler "\"}], ") 
        property (str filler "\"owl:onProperty\": [{\"object\": \"" property "\"}], ")
        rdfType (str property "\"rdf:type\": [{\"object\": \"owl:Restriction\"}]")
        closing (str rdfType "}")]
    closing))

(defn translateObjectHasSelf
  "Translate a ObjectHasValue expression"
  [ofn]
  (let [[op property] ofn
        hasValue "{\"owl:hasSelf\": "
        filler (str hasValue "[{\"object\": \"true^^xsd:boolean\"}], ") 
        property (str filler "\"owl:onProperty\": [{\"object\": \"" property "\"}], ")
        rdfType (str property "\"rdf:type\": [{\"object\": \"owl:Restriction\"}]")
        closing (str rdfType "}")]
    closing))


(defn translateObjectMinUnqualifiedCardinality
  "Translate a ObjectMinCardinality expression"
  [ofn]
  (let [[op cardinality property] ofn
        minCard "{\"owl:minCardinality\": "
        number (str minCard "[{\"object\": \"" cardinality "^^xsd:nonNegativeInteger\"}], ") 
        property (str number "\"owl:onProperty\": [{\"object\": \"" property "\"}], ")
        rdfType (str property "\"rdf:type\": [{\"object\": \"owl:Restriction\"}]")
        closing (str rdfType "}")]
    closing))

(defn translateObjectMinQualifiedCardinality
  "Translate a ObjectMinQualifiedCardinality expression"
  [ofn]
  (let [[op cardinality property filler] ofn
        minCard "{\"owl:minCardinality\": "
        number (str minCard "[{\"object\": \"" cardinality "^^xsd:nonNegativeInteger\"}], ") 
        property (str number "\"owl:onProperty\": [{\"object\": \"" property "\"}], ")
        filler (str property "\"owl:onClass\": [{\"object\": \"" filler "\"}], ")
        rdfType (str filler "\"rdf:type\": [{\"object\": \"owl:Restriction\"}]")
        closing (str rdfType "}")]
    closing))

(defn translateObjectMinCardinality
  "Translate a ObjectMinCardinality expression"
  [ofn]
  (if (= 3 (count ofn))
    (translateObjectMinUnqualifiedCardinality ofn)
    (translateObjectMinQualifiedCardinality ofn)))


(defn translateObjectMaxUnqualifiedCardinality
  "Translate a ObjectMaxCardinality expression"
  [ofn]
  (let [[op cardinality property] ofn
        maxCard "{\"owl:maxCardinality\": "
        number (str maxCard "[{\"object\": \"" cardinality "^^xsd:nonNegativeInteger\"}], ") 
        property (str number "\"owl:onProperty\": [{\"object\": \"" property "\"}], ")
        rdfType (str property "\"rdf:type\": [{\"object\": \"owl:Restriction\"}]")
        closing (str rdfType "}")]
    closing))

(defn translateObjectMaxQualifiedCardinality
  "Translate a ObjectMaxQualifiedCardinality expression"
  [ofn]
  (let [[op cardinality property filler] ofn
        maxCard "{\"owl:maxCardinality\": "
        number (str maxCard "[{\"object\": \"" cardinality "^^xsd:nonNegativeInteger\"}], ") 
        property (str number "\"owl:onProperty\": [{\"object\": \"" property "\"}], ")
        filler (str property "\"owl:onClass\": [{\"object\": \"" filler "\"}], ")
        rdfType (str filler "\"rdf:type\": [{\"object\": \"owl:Restriction\"}]")
        closing (str rdfType "}")]
    closing))

(defn translateObjectMaxCardinality
  "Translate a ObjectMaxCardinality expression"
  [ofn]
  (if (= 3 (count ofn))
    (translateObjectMaxUnqualifiedCardinality ofn)
    (translateObjectMaxQualifiedCardinality ofn)))

(defn translateObjectExactUnqualifiedCardinality
  "Translate a ObjectExactCardinality expression"
  [ofn]
  (let [[op cardinality property] ofn
        maxCard "{\"owl:cardinality\": "
        number (str maxCard "[{\"object\": \"" cardinality "^^xsd:nonNegativeInteger\"}], ") 
        property (str number "\"owl:onProperty\": [{\"object\": \"" property "\"}], ")
        rdfType (str property "\"rdf:type\": [{\"object\": \"owl:Restriction\"}]")
        closing (str rdfType "}")]
    closing))

(defn translateObjectExactQualifiedCardinality
  "Translate a ObjectExactQualifiedCardinality expression"
  [ofn]
  (let [[op cardinality property filler] ofn
        maxCard "{\"owl:qualifiedCardinality\": "
        number (str maxCard "[{\"object\": \"" cardinality "^^xsd:nonNegativeInteger\"}], ") 
        property (str number "\"owl:onProperty\": [{\"object\": \"" property "\"}], ")
        filler (str property "\"owl:onClass\": [{\"object\": \"" filler "\"}], ")
        rdfType (str filler "\"rdf:type\": [{\"object\": \"owl:Restriction\"}]")
        closing (str rdfType "}")]
    closing))

(defn translateObjectExactCardinality
  "Translate a ObjectExactCardinality expression"
  [ofn]
  (if (= 3 (count ofn))
    (translateObjectExactUnqualifiedCardinality ofn)
    (translateObjectExactQualifiedCardinality ofn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Propositional Connectives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateObjectIntersection
  "Translate an ObjectIntersectionOf expression"
  [ofn]
  (let [[operator & arguments] ofn
        intersection "{\"owl:intersectionOf\": "
        operands (str intersection "[{\"object\": " (translateList arguments) "}]") 
        rdfType (str operands ", \"rdf:type\": [{\"object\": \"owl:Class\"}]")
        closing (str rdfType "}")]
    closing)) 

(defn translateObjectUnion
  "Translate an ObjectIntersectionOf expression"
  [ofn]
  (let [[operator & arguments] ofn
        union "{\"owl:unionOf\": "
        operands (str union "[{\"object\": " (translateList arguments) "}]") 
        rdfType (str operands ", \"rdf:type\": [{\"object\": \"owl:Class\"}]")
        closing (str rdfType "}")]
    closing)) 

(defn translateObjectOneOf
  "Translate an ObjectOneOf expression"
  [ofn]
  (let [[operator & arguments] ofn
        oneOf "{\"owl:oneOf\": "
        operands (str oneOf "[{\"object\": " (translateList arguments) "}]") ;TODO: translation for individuals
        rdfType (str operands ", \"rdf:type\": [{\"object\": \"owl:Class\"}]")
        closing (str rdfType "}")]
    closing)) 

(defn translateObjectComplement
  "Translate an ObjectComplementOf expression"
  [ofn]
  (let [[operator argument] ofn
        compl "{\"owl:complementOf\": "
        operand (str compl "[{\"object\": " (translate argument) "}]")
        rdfType (str operand ", \"rdf:type\": [{\"object\": \"owl:Class\"}]")
        closing (str rdfType "}")]
    closing))

(defn translate
  "Translate OFN-S expression to tick triple"
  [ofn]
  (let [operator (first ofn)];
    (case operator
      "ObjectSomeValuesFrom"  (translateObjectSomeValuesFrom ofn)
      "ObjectAllValuesFrom"  (translateObjectAllValuesFrom ofn)
      "ObjectHasValue"  (translateObjectHasValue ofn)
      "ObjectMinCardinality"  (translateObjectMinCardinality ofn)
      "ObjectMaxCardinality"  (translateObjectMaxCardinality ofn)
      "ObjectExactCardinality"  (translateObjectMaxCardinality ofn)
      "ObjectIntersectionOf"  (translateObjectIntersection ofn)
      "ObjectUnionOf"  (translateObjectUnion ofn)
      "ObjectOneOf"  (translateObjectOneOf ofn)
      "ObjectComplementOf"  (translateObjectComplement ofn)
      (str \" ofn \"))))

