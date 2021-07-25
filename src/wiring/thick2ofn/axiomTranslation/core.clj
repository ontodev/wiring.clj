(ns wiring.thick2ofn.axiomTranslation.core
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [wiring.thick2ofn.parsing :as p]
            [cheshire.core :as cs]
            [wiring.thick2ofn.axiomTranslation.translate :as t]
            [wiring.thick2ofn.spec :as spec])
  (:gen-class))

(defn -main
  "Currently only used for manual testing."
  [& args]

  (with-open [rdr (io/reader (io/resource "tests/thickOBO.txt"))]
    (doseq [line (line-seq rdr)] 
      (println (str "Input: " line))
      (println (str "Output: " (cs/generate-string (t/translate (p/parse line)))))
      (println ""))))

