(ns wiring.thick2ofn.dataPropertyTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.thick2ofn.util :as util]
            [wiring.thick2ofn.spec :as owlspec]))

(declare translate) ;recursive parsing (not tail recursive)  

(defn translate
  "Translate property map containing a property expression to OFS."
  [predicateMap]
  (if (string? predicateMap)
    (str "\"" predicateMap "\"") ;base case 
    (str "Error for " predicateMap))) ;data properties are always atomic
