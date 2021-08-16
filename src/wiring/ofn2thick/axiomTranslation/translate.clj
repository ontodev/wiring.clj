(ns wiring.ofn2thick.axiomTranslation.translate
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2thick.expressionTranslation.classTranslation :as classTranslation]
            [wiring.ofn2thick.axiomTranslation.classExpressionAxiom :as CEA]
            [wiring.ofn2thick.axiomTranslation.objectPropertyAxioms :as PEA]
            [wiring.ofn2thick.spec :as owlspec])
  (:gen-class))

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
  [ofn]
  (let [operator (first ofn)]; 
    (case operator

      ;class expression axioms
      "SubClassOf" (CEA/translateSubclassOf ofn)
      "DisjointUnion" (CEA/translateDisjointUnion ofn)
      "DisjointClasses" (CEA/translateDisjointClasses ofn)
      "EquivalentClasses" (CEA/translateEquivalentClasses ofn)
      
      ;TODO object property axioms
      "SubObjectPropertyOf" (PEA/translateSubObjectPropertyOf ofn)
      "ObjectPropertyChain" (PEA/translatePropertyChainAxiom ofn)
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
