(ns wiring.ofn2ldtab.axiomTranslation.translate
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2ldtab.expressionTranslation.classTranslation :as classTranslation]
            [wiring.ofn2ldtab.axiomTranslation.classExpressionAxiom :as CEA]
            [wiring.ofn2ldtab.axiomTranslation.objectPropertyAxioms :as PEA]
            [wiring.ofn2ldtab.spec :as owlspec])
  (:gen-class))

;assertion ;TODO
;retraction
;graph
;subject
;predicate
;object
;datatype ;TODO
;annotation ;TODO

;TODO; make sure generated blank nodes are unique 
;(this is best done in a post-processing step I guess)
(declare translate)

(defn translateThinTriple
  "Translate OFN-S thin triple to thick triple thin triple"
  [ofn]
  ;TODO
  (let [[op s p o] ofn
        triple {:subject s
                :predicate p
                :object o}]
    triple))

(defn translateClassAssertion
  "Translate OFN-S class assertions into thick triples"
  [ofn]
  ;TODO
  (let [[op s o] ofn
        triple {:subject s
                :predicate "rdf:type"
                :object (classTranslation/translate o)}]
    triple))

(defn translate
  "Translate OFN-S expression to thick triple"
  [ofn graph]
  (let [operator (first ofn)]; 
    (case operator

      ;class expression axioms
      "SubClassOf" (CEA/translateSubclassOf ofn graph)
      "DisjointUnion" (CEA/translateDisjointUnion ofn graph)
      "DisjointClasses" (CEA/translateDisjointClasses ofn graph)
      "EquivalentClasses" (CEA/translateEquivalentClasses ofn graph)

      ;TODO object property axioms
      "SubObjectPropertyOf" (PEA/translateSubObjectPropertyOf ofn)
      "ObjectPropertyDomain" (PEA/translateDomain ofn)
      "ObjectPropertyRange" (PEA/translateRange ofn)
      "InverseObjectProperties" (PEA/translateInverseOf ofn)

      "InverseFunctionalObjectProperty" (PEA/translateInverseFunctionalProperty ofn)
      "ReflexiveObjectProperty" (PEA/translateReflexiveProperty ofn)
      "IrreflexiveObjectProperty" (PEA/translateIrreflexiveProperty ofn)
      "AsymmetricObjectProperty" (PEA/translateAsymmetricProperty ofn)
      "SymmetricObjectProperty" (PEA/translateSymmetricProperty ofn)
      "TransitiveObjectProperty" (PEA/translateTransitiveProperty ofn)
      "ObjectDisjointProperties" (PEA/translateDisjointProperties ofn)

      ;ambiguous property axioms
      "PropertyDomain" (PEA/translateDomain ofn)
      "PropertyRange" (PEA/translateRange ofn)
      "SubPropertyOf" (PEA/translateSubObjectPropertyOf ofn)
      "FunctionalProperty" (PEA/translateFunctionalProperty ofn)
      "DisjointProperties" (PEA/translateDisjointProperties ofn)

      ;TODO data property axioms

      "ClassAssertion" (translateClassAssertion ofn)
      "ThinTriple" (translateThinTriple ofn)
      (str "ERROR: " ofn))))
