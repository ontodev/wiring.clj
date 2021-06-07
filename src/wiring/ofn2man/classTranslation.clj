(ns wiring.ofn2man.classTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [wiring.ofn2man.propertyTranslation :as property])
            ;[wiring.thick2ofn.spec :as spec])
  (:gen-class))

;TODO pretty printing with line breaks and indentations

;TODO data validation
(declare translate)

;TODO nested property expressions
(defn namedClass?
  "Checks whether an expression is a named class."
  [ofn]
  (let [operator (first ofn)];
    (case operator
      "ObjectSomeValuesFrom" false
      "ObjectAllValuesFrom" false
      "ObjectHasValue"  false
      "ObjectMinCardinality" false
      "ObjectMaxCardinality" false
      "ObjectExactCardinality" false
      "ObjectIntersectionOf" false
      "ObjectUnionOf" false
      "ObjectOneOf" false
      "ObjectComplementOf" false
      true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Restrictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO: rethink handling of parentheses for (nested) class expressions
(defn translateObjectSomeValuesFrom
  "Translate a ObjectSomeValuesFrom expression"
  [ofn]
  (let [[op property filler] ofn]
    (if (namedClass? filler)
      (str (property/translate property) " some " filler)
      (str (property/translate property) " some (" (translate filler) ")"))))

(defn translateObjectAllValuesFrom
  "Translate a ObjectAllValuesFrom expression"
  [ofn]
  (let [[op property filler] ofn]
    (if (namedClass? filler)
      (str (property/translate property) " only " filler)
      (str (property/translate property) " only (" (translate filler) ")"))))

(defn translateObjectHasValue
  "Translate a ObjectHasValue expression"
  [ofn]
  (let [[op property filler] ofn]
    (str (property/translate property) " value " filler)));no nesting for filler because it's an individual

(defn translateObjectMinUnqualifiedCardinality
  "Translate a ObjectMinCardinality expression"
  [ofn]
  (let [[op cardinality property] ofn]
    (str (property/translate property) " min " cardinality " owl:Thing")))

(defn translateObjectMinQualifiedCardinality
  "Translate a ObjectMinQualifiedCardinality expression"
  [ofn]
  (let [[op cardinality property filler] ofn]
    (if (namedClass? filler)
      (str (property/translate property) " min " cardinality " " filler)
      (str (property/translate property) " min " cardinality " (" (translate filler) ")"))))

(defn translateObjectMinCardinality
  "Translate a ObjectMinCardinality expression"
  [ofn]
  (if (= 3 (count ofn))
    (translateObjectMinUnqualifiedCardinality ofn)
    (translateObjectMinQualifiedCardinality ofn)))

(defn translateObjectMaxUnqualifiedCardinality
  "Translate a ObjectMaxCardinality expression"
  [ofn]
  (let [[op cardinality property] ofn]
    (str (property/translate property) " max " cardinality " owl:Thing")))

(defn translateObjectMaxQualifiedCardinality
  "Translate a ObjectMaxQualifiedCardinality expression"
  [ofn]
  (let [[op cardinality property filler] ofn]
    (if (namedClass? filler)
      (str (property/translate property) " max " cardinality " " filler)
      (str (property/translate property) " max " cardinality " (" (translate filler) ")"))))

(defn translateObjectMaxCardinality
  "Translate a ObjectMaxCardinality expression"
  [ofn]
  (if (= 3 (count ofn))
    (translateObjectMaxUnqualifiedCardinality ofn)
    (translateObjectMaxQualifiedCardinality ofn)))

(defn translateObjectExactUnqualifiedCardinality
  "Translate a ObjectExactCardinality expression"
  [ofn]
  (let [[op cardinality property] ofn]
    (str (property/translate property) " exactly " cardinality " owl:Thing")))

(defn translateObjectExactQualifiedCardinality
  "Translate a ObjectExactQualifiedCardinality expression"
  [ofn]
  (let [[op cardinality property filler] ofn]
    (if (namedClass? filler)
      (str (property/translate property) " exactly " cardinality " " filler)
      (str (property/translate property) " exactly " cardinality " (" (translate filler) ")"))))

(defn translateObjectExactCardinality
  "Translate a ObjectExactCardinality expression"
  [ofn]
  (if (= 3 (count ofn))
    (translateObjectExactUnqualifiedCardinality ofn)
    (translateObjectExactQualifiedCardinality ofn)))

(defn translateObjectHasSelf
  "Translate a ObjectHasValue expression"
  [ofn]
  (let [[op property] ofn]
    (str (property/translate property) " some Self")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             Propositional Connectives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateObjectIntersection
  "Translate an ObjectIntersectionOf expression"
  [ofn]
  (let [[operator & arguments] ofn
        args (map #(if (namedClass? %) % (str "(" (translate %) ")")) arguments)]
    (apply str (interpose " and " args))))

(defn translateObjectUnion
  "Translate an ObjectUnionOf expression"
  [ofn]
  (let [[operator & arguments] ofn
        args (map #(if (namedClass? %) % (str "(" (translate %) ")")) arguments)]
    (apply str (interpose " or " args))))

(defn translateObjectOneOf
  "Translate an ObjectOneOf expression"
  [ofn]
  (let [[operator & arguments] ofn]
    (str "{" (apply str (interpose " , " arguments)) "}")))

(defn translateObjectComplement
  "Translate an ObjectComplementOf expression"
  [ofn]
  (let [[operator argument] ofn]
    (str "not (" (translate argument) ")")));NB: there are parenthesis for named classes too

(defn translate
  "Translate OFN-S expression to manchester synax"
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
      "ObjectHasSelf"  (translateObjectHasSelf ofn)
      ofn)))

