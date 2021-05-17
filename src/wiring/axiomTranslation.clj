(ns wiring.axiomTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.propertyTranslation :as propertyTranslation]
            [wiring.classTranslation :as classTranslation]
            [wiring.util :as util]
            [wiring.spec :as owlspec]))

(declare translate)

;TODO
(defn translateSubclassOf 
  "Translate a SubClassOf axiom"   
  [predicates]
  (let [subclass (classTranslation/translate (:subject predicates))
        superclass (classTranslation/translate (:object predicates))]
    (util/ofsFormat "SubClassOf" subclass superclass)))

