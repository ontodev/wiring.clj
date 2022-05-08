(ns wiring.ofn2ldtab.core
  (:require [clojure.repl :as repl]
            [wiring.ofn2ldtab.axiomTranslation.translate :as t]
            [wiring.ofn2ldtab.util :as u]
            [cheshire.core :as cs]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main
  "Currently only used for manual testing."
  [& args]
  ;(def subclassOf "[\"SubClassOf\",\"ex:A\",\"ex:B\"]")
  ;(def subclassOf "[\"SubClassOf\",\"ex:A\",\"<sd>\"]")
  ;(def subclassof "[\"subclassof\",\"ex:a\",\"\\\"asd\\\"^^xsd:string\"]")
  (def subclassOf "[\"SubClassOf\",\"ex:A\",\"\\\"asd\\\"@en\"]")
  (def someValuesFrom "[\"SubClassOf\",\"ex:A\",[\"ObjectSomeValuesFrom\",\"ex:R\",\"ex:B\"]]")
  (def intersection "[\"SubClassOf\",\"ex:A\",[\"ObjectIntersectionOf\",\"ex:R\",\"ex:R\",\"ex:A\"]]")

  (def a (cs/generate-string (t/translate (cs/parse-string subclassOf) "graph")))
  (println a)

  (def b (cs/generate-string (t/translate (cs/parse-string intersection) "graph")))
  (println b)


  ;(println (u/get-datetype "\"asd\""))
  ;(println (u/get-datetype "<asd>")) 
  ;(println (u/get-datetype "asd:aaa"))
  ;(println (u/has-language-tag "\"asd\"@en"))
  ;(println (u/has-datatype "\"asd\"^^xsd:string"))
  ;(println (u/has-datatype "\"asd\"@en"))
  ;(println (u/has-language-tag "\"asd\"^^xsd:string"))

  )
