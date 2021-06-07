(ns wiring.ofn2thick.spec
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]
            [clojure.spec.alpha :as spec]))

(spec/def ::classExpression (spec/or :namedClass string?
                                  :classConstructor coll?))

(spec/def ::propertyExpression (spec/or :namedProperty string?
                                     :propertyConstructor coll?))
(spec/def ::individual string?)

(defn string-or-coll? [x]
  (or (string? x) (coll? x)))

(defn string-Number? [x]
  (number? (read-string x))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      RDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Note: parsing an OFN-S expression as JSON returns a seq.
;So, a 'list' of arguments will be a seq.
(spec/def ::list seq?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Restrictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::someValuesFrom (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(= % "ObjectSomeValuesFrom") :property ::propertyExpression :filler ::classExpression)))

(spec/def ::allValuesFrom (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(= % "ObjectAllValuesFrom") :property ::propertyExpression :filler ::classExpression)))

(spec/def ::hasValue (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(= % "ObjectHasValue") :property ::propertyExpression :filler ::classExpression)))

(spec/def ::hasSelf (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 2)
                             (spec/cat :operator #(= % "ObjectHasSelf") :property ::propertyExpression)))

(spec/def ::minCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(= % "ObjectMinCardinality") :cardinality string-Number? :property ::propertyExpression)))

(spec/def ::minQualifiedCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 4)
                             (spec/cat :operator #(= % "ObjectMinCardinality") :cardinality string-Number? :property ::propertyExpression :filler ::classExpression)))

(spec/def ::maxCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(= % "ObjectMaxCardinality") :cardinality string-Number? :property ::propertyExpression)))

(spec/def ::maxQualifiedCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 4)
                             (spec/cat :operator #(= % "ObjectMaxCardinality") :cardinality string-Number? :property ::propertyExpression :filler ::classExpression)))

(spec/def ::exactQualifiedCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 4)
                             (spec/cat :operator #(= % "ObjectExactQualifiedCardinality") :cardinality string-Number? :property ::propertyExpression :filler ::classExpression)))

(spec/def ::exactCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(= % "ObjectExactQualifiedCardinality") :cardinality string-Number? :property ::propertyExpression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Propositional Connectives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(spec/def ::classIntersection (spec/and 
                             (spec/coll-of string-or-coll? :kind seq)
                             (spec/cat :operator #(= % "ObjectIntersectionOf") :arguments (spec/* ::classExpression))))

(spec/def ::classUnion (spec/and 
                             (spec/coll-of string-or-coll? :kind seq)
                             (spec/cat :operator #(= % "ObjectUnionOf") :arguments (spec/* ::classExpression))))

(spec/def ::classOneOf (spec/and 
                             (spec/coll-of string-or-coll? :kind seq)
                             (spec/cat :operator #(= % "ObjectOneOf") :arguments (spec/* ::individual))))

(spec/def ::classComplement (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 2)
                             (spec/cat :operator #(= % "ObjectComplementOf") :argument ::classExpression))) 
