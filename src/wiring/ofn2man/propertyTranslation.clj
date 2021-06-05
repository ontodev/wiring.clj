(ns wiring.ofn2man.propertyTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io])
  (:gen-class))

;TODO data validation
(declare translate) 

(defn translateInverseOf 
  "Translate ObjectInverseOf expression"
  [ofn]
  (let [[op arg] ofn]
      (str "inverse (" (translate arg) ")")))

(defn translate 
  "Translate OFN-S property expression into predicate map"
  [ofn]
  (println ofn)
  (if (= "ObjectInverseOf" (first ofn))
    (translateInverseOf ofn)
     ofn))
