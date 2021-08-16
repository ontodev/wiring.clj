(ns wiring.thick2ofn.loading
  (:require [clojure.repl :as repl]
            [wiring.thick2ofn.parsing :as p]
            [clojure.java.io :as io]
            [wiring.thick2ofn.axiomTranslation.translate :as thick2ofn]))

(defn loadOntology
  "Loads an ontology reprented in thick triples and returns the corresponding
  collection of OFN-S expressions"
  [path]
  (->> path
       io/reader 
       line-seq
       (map p/parse)
       (map thick2ofn/translate)))
