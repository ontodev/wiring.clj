(ns wiring.ofn2thick.propertyTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [wiring.thick2ofn.spec :as spec])
  (:gen-class))

;TODO data validation
(declare translate)

(defn translateInverseOf
  "Translate ObjectInverseOf expression"
  [ofn]
  (let [[op arg] ofn
        inverse "{\"owl:inverseOf\": "
        operand (str inverse "[{\"object\": " (translate arg) "}]")
        closing (str operand "}")]
    closing))

(defn translate
  "Translate OFN-S property expression into predicate map"
  [ofn]
  ;(println ofn)
  (if (= "ObjectInverseOf" (first ofn))
    (translateInverseOf ofn)
    (str "\"" ofn "\"")))
