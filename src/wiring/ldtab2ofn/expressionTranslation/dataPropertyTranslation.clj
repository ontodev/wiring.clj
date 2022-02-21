(ns wiring.ldtab2ofn.dataPropertyTranslation
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [wiring.ldtab2ofn.util :as util]
            [wiring.ldtab2ofn.spec :as owlspec]))

(declare translate) ;recursive parsing (not tail recursive)  

(defn translate
  "Translate property map containing a property expression to OFS."
  [predicateMap]
  (if (string? predicateMap)
    (str "\"" predicateMap "\"") ;base case 
    (str "Error for " predicateMap))) ;data properties are always atomic
