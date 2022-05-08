(ns wiring.ofn2ldtab.spec
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
;;                Class Restrictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spec/def ::someValuesFrom (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(or (= % "ObjectSomeValuesFrom")
                                                     (= % "SomeValuesFrom"))
                                       :property ::propertyExpression :filler ::classExpression)))

(spec/def ::allValuesFrom (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(or (= % "ObjectAllValuesFrom")
                                                     (= % "AllValuesFrom"))
                                       :property ::propertyExpression :filler ::classExpression)))

(spec/def ::hasValue (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(or (= % "ObjectHasValue")
                                                     (= % "HasValue"))
                                       :property ::propertyExpression :filler ::classExpression)))

(spec/def ::hasSelf (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 2)
                             (spec/cat :operator #(= % "ObjectHasSelf") :property ::propertyExpression)))

(spec/def ::minCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(or (= % "ObjectMinCardinality")
                                                     (= % "MinCardinality"))
                                       :cardinality string-Number? :property ::propertyExpression)))

(spec/def ::minQualifiedCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 4)
                             (spec/cat :operator #(= % "ObjectMinCardinality") :cardinality string-Number? :property ::propertyExpression :filler ::classExpression)))

(spec/def ::maxCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(or (= % "ObjectMaxCardinality")
                                                     (= % "MaxCardinality"))
                                       :cardinality string-Number? :property ::propertyExpression)))

(spec/def ::maxQualifiedCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 4)
                             (spec/cat :operator #(= % "ObjectMaxCardinality") :cardinality string-Number? :property ::propertyExpression :filler ::classExpression)))

(spec/def ::exactQualifiedCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 4)
                             (spec/cat :operator #(= % "ObjectExactCardinality") :cardinality string-Number? :property ::propertyExpression :filler ::classExpression)))

(spec/def ::exactCardinality (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(or (= % "ObjectExactCardinality")
                                                     (= % "ExactCardinality"))
                                       :cardinality string-Number? :property ::propertyExpression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Propositional Connectives for Class Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(spec/def ::classIntersection (spec/and 
                             (spec/coll-of string-or-coll? :kind seq)
                             (spec/cat :operator #(= % "ObjectIntersectionOf") :arguments (spec/* ::classExpression))))

(spec/def ::classUnion (spec/and 
                             (spec/coll-of string-or-coll? :kind seq)
                             (spec/cat :operator #(= % "ObjectUnionOf") :arguments (spec/* ::classExpression))))

(spec/def ::oneOf (spec/and 
                             (spec/coll-of string-or-coll? :kind seq)
                             (spec/cat :operator #(or (= % "ObjectOneOf")
                                                     (= % "OneOf"))
                                       :arguments (spec/* ::individual))))

(spec/def ::classComplement (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 2)
                             (spec/cat :operator #(= % "ObjectComplementOf") :argument ::classExpression))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Object Property Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(spec/def ::inverseOf (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 2)
                             (spec/cat :operator #(= % "ObjectInverseOf") :argument ::propertyExpression))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Class Expression Axioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(spec/def ::subclassOf (spec/and 
                             (spec/coll-of string-or-coll? :kind seq :count 3)
                             (spec/cat :operator #(= % "SubClassOf") :leftHandSide ::classExpression :rightHandSide ::classExpression))) 

(spec/def ::equivalentClasses (spec/and 
                             (spec/coll-of string-or-coll? :kind seq)
                             (spec/cat :operator #(= % "EquivalentClasses") :arguments (spec/* ::classExpression))))


(spec/def ::disjointClasses (spec/and 
                             (spec/coll-of string-or-coll? :kind seq)
                             (spec/cat :operator #(= % "DisjointClasses") :arguments (spec/* ::classExpression))))

;Note that the 'left-hand side' class of a DisjointUnion axiom is validated
;in the same way as the 'right-hand side' classes
(spec/def ::disjointUnion (spec/and 
                             (spec/coll-of string-or-coll? :kind seq)
                             (spec/cat :operator #(= % "DisjointUnion") :arguments (spec/* ::classExpression))))

