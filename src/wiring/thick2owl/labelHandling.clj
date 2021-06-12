(ns wiring.thick2owl.labelHandling
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]
            [wiring.util.ofnExpressions :as ofnExpressions]
            [wiring.thick2ofn.util :as ofnUtil];TODO: refactor this
            [clojure.spec.alpha :as spec]))

(declare translate)

(defn render 
  "Rendering of a label L either as L or as 'L' in case L includes white spaces."
  [label]
  (let [hasBlank (some #(s/blank? (str %)) (set label))]
    (if hasBlank
      (str \" "'" label "'" \")
      (str \" label \"))))

(defn id
  "Identity function for OFN-S expressions."
  [ofn subject2label]
  (if (ofnExpressions/namedEntity? ofn)
    (if (contains? subject2label ofn)
      (render (first (get subject2label ofn)))
      (render ofn))
    (let [[operator & arguments] ofn
           args (map #(translate % subject2label) arguments)
           op (ofnUtil/ofsFormat operator (apply ofnUtil/ofsFormatNoBrackets args))]
      op))) 

(defn translate
  "Translate (abstract) OFN-S expression to an OFN-S expression"
  [ofn subject2label]
  (id ofn subject2label))
