(ns wiring.labelling.core
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [cheshire.core :as cs]
            [wiring.thick2ofn.axiomTranslation.translate :as thick2ofn]
            [wiring.labelling.labelHandling :as labelHandling]
            [wiring.thick2ofn.parsing :as p]
            [wiring.thick2ofn.loading :as l]
            [wiring.util.ofnExpressions :as ofnExpressions]
            ;[wiring.thick2ofn.axiomTranslation.translate :as thick2ofn]
            [wiring.ofn2man.axiomTranslation :as ofn2man]
            [wiring.util.thickTriples :as thickTriples]
            [clojure.spec.alpha :as spec]))

(defn -main
  "Currently only used for manual testing."
  [& args] 

  ;translate ontology given via thick triples into OFN-S expressions
  (def ofnExpressions (->> "tests/thickOBO.txt"
       io/resource
       io/reader 
       line-seq
       (map p/parse)
       (map thick2ofn/translate)))

  ;(def ofnExpressions (l/loadOntology "testPath")) 

  (def labelling (labelHandling/inlineLabels ofnExpressions))
  (println labelling)) 
