(ns wiring.typing.core
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [wiring.thick2ofn.parsing :as p]
            [clojure.java.io :as io]
            [wiring.thick2ofn.axiomTranslation.translate :as thick2ofn]
            [wiring.typing.typeHandling :as typeHandling]
            [wiring.util.thickTriples :as thickTriples]
            [wiring.util.ofnExpressions :as ofnExpressions]))

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

  (println ofnExpressions)
  (println "")

  (def typing (typeHandling/typingUpdate ofnExpressions))
  (println typing)

  )
