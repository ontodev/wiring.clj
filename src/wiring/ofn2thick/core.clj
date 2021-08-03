(ns wiring.ofn2thick.core
  (:require [clojure.repl :as repl]
            [wiring.ofn2thick.axiomTranslation :as axiomTranslation]
            [wiring.thick2ofn.axiomTranslation.translate :as t]
            [wiring.thick2ofn.parsing :as p]
            [cheshire.core :as cs]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main
  "Currently only used for manual testing."
  [& args]
  (def subclassOf "[\"SubClassOf\",\"ex:A\",\"ex:B\"]")
  ;(def subclassOf "[\"SubClassOf\",\"ex:A\",[\"ObjectSomeValuesFrom\",\"ex:R\",\"ex:B\"]]")
  (def disjointUnion "[\"DisjointUnion\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")
  (def disjointClasses "[\"DisjointClasses\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")
  (def equivalentClasses "[\"EquivalentClasses\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")

  (def hasVaue "[\"SubClassOf\",\"ex:A\",[\"ObjectHasValue\",\"ex:R\",\"ex:B\"]]")
  (def minCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMinCardinality\",\"1\",\"ex:R\"]]")
  (def minQualifiedCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMinCardinality\",\"1\",\"ex:R\",\"ex:A\"]]")
  (def maxCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMaxCardinality\",\"1\",\"ex:R\"]]")
  (def maxQualifiedCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMaxCardinality\",\"1\",\"ex:R\",\"ex:A\"]]")
  (def intersection "[\"SubClassOf\",\"ex:A\",[\"ObjectIntersectionOf\",\"1\",\"ex:R\",\"ex:A\"]]")
  (def union "[\"SubClassOf\",\"ex:A\",[\"ObjectUnionOf\",\"1\",\"ex:R\",\"ex:A\"]]")
  (def complemen "[\"SubClassOf\",\"ex:A\",[\"ObjectComplementOf\",\"ex:A\"]]")
  (def inverse "[\"SubClassOf\",\"ex:A\",[\"ObjectSomeValuesFrom\",[\"ObjectInverseOf\",\"ex:R\"],[\"ObjectSomeValuesFrom\",\"ex:R\",\"ex:B\"]]]")

  ;(println (cs/parse-string subclassOf)) 
  ;(println (cs/generate-string (axiomTranslation/translate (cs/parse-string subclassOf))))
  ;(def a (cs/generate-string (axiomTranslation/translate (cs/parse-string disjointUnion))))
  ;(println a)
  ;(println (t/translate (p/parse a)))

  ;(def b (cs/generate-string (axiomTranslation/translate (cs/parse-string disjointClasses))))
  ;(println b)
  ;(println (t/translate (p/parse b)))

  ;(def c (cs/generate-string (axiomTranslation/translate (cs/parse-string equivalentClasses))))
  ;;(println c)
  ;(println (t/translate (p/parse c)))

  ;(println (cs/generate-string (axiomTranslation/translate (cs/parse-string hasVaue))))

  ;(println minCard)
  ;(def b (cs/generate-string (axiomTranslation/translate (cs/parse-string minCard))))
  ;(println (cs/generate-string (t/translate (p/parse b))))

  ;(println minQualifiedCard)
  ;(println (axiomTranslation/translate (cs/parse-string minQualifiedCard)))
  ;(def b (cs/generate-string (axiomTranslation/translate (cs/parse-string minQualifiedCard))))
  ;(println (cs/generate-string (t/translate (p/parse b))))

  ;(println (cs/generate-string (axiomTranslation/translate (cs/parse-string maxCard))))


  (println maxQualifiedCard)
  (def a (cs/generate-string (axiomTranslation/translate (cs/parse-string maxQualifiedCard))))
  (println (cs/generate-string (t/translate (p/parse a))))

  ;(println (cs/generate-string (axiomTranslation/translate (cs/parse-string intersection))))
  ;(println (cs/generate-string (axiomTranslation/translate (cs/parse-string union))))
  ;(println (cs/generate-string (axiomTranslation/translate (cs/parse-string complemen))))
  ;(println (cs/generate-string (axiomTranslation/translate (cs/parse-string inverse))))

  (println "") 
  (def thin "[\"ThinTriple\",\"s\",\"p\",\"o\"]") 
  (println (axiomTranslation/translate (cs/parse-string thin))) 
  (def ts {:subject "s" :predicate "p" :object "o"})
  (def aa (cs/generate-string ts))
  (println aa) 

  )
