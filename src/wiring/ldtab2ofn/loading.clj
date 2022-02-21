ldtabiring.ldtab2ofn.loading
  (:require [clojure.repl :as repl]
            [wiring.ldtab2ofn.parsing :as p]
            [clojure.java.io :as io]
            [wiring.ldtab2ofn.axiomTranslation.translate :as ldtab2ofn]))

(defn loadOntology
  "Loads an ontology reprented in thick triples and returns the corresponding
  collection of OFN-S expressions"
  [path]
  (->> path
       io/reader 
       line-seq
       (map p/parse)
       (map ldtab2ofn/translate)))
