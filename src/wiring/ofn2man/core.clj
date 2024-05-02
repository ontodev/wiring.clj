(ns wiring.ofn2man.core
  (:require [clojure.repl :as repl]
            [cheshire.core :as cs]
            [wiring.ofn2man.axiomTranslation :as axiomTranslation]
            [clojure.java.io :as io])
  (:gen-class))

;NB: ofn2man will only be used for self-contained expressions
;in particular, class frames are not treated because
;a class frame cannot be represented as an OFN-S expression 
;however, a thick triple captures class frames 
;so ofn2man will only be used as an intermediate step to 
;translate between thick and man 

(defn -main
  "Currently only used for manual testing."
  [& args]

  (def subclassOf "[\"SubClassOf\",\"ex:A\",[\"ObjectSomeValuesFrom\",\"ex:R\",\"ex:B\"]]")
  (def disjointUnion "[\"DisjointUnion\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")
  (def disjointClasses "[\"DisjointClasses\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")
  (def equivalentClasses "[\"EquivalentClasses\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")

  (def hasVaue "[\"SubClassOf\",\"ex:A\",[\"ObjectHasValue\",\"ex:R\",\"ex:B\"]]")
  (def minCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMinCardinality\",\"1\",\"ex:R\"]]")
  (def minQualifiedCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMinCardinality\",\"1\",\"ex:R\",\"ex:A\"]]")
  (def maxCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMaxCardinality\",\"1\",\"ex:R\"]]")
  (def maxQualifiedCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMaxCardinality\",\"1\",\"ex:R\",\"ex:A\"]]")
  (def intersection "[\"SubClassOf\",\"ex:A\",[\"ObjectIntersectionOf\",\"1\",\"ex:R\",\"ex:A\"]]")
  (def intersection2 "[\"SubClassOf\",\"ex:A\",[\"ObjectIntersectionOf\",[\"ObjectSomeValuesFrom\",\"ex:R\",\"ex:B\"],\"ex:R\",\"ex:A\"]]")
  (def union "[\"SubClassOf\",\"ex:A\",[\"ObjectUnionOf\",\"1\",\"ex:R\",\"ex:A\"]]")
  (def complemen "[\"SubClassOf\",\"ex:A\",[\"ObjectComplementOf\",\"ex:A\"]]")
  (def inverse "[\"SubClassOf\",\"ex:A\",[\"ObjectSomeValuesFrom\",[\"ObjectInverseOf\",\"ex:R\"],[\"ObjectSomeValuesFrom\",\"ex:R\",\"ex:B\"]]]")
  (def nesting "[\"SubClassOf\",\"ex:A\",[\"ObjectSomeValuesFrom\",\"ex:R\",[\"ObjectSomeValuesFrom\",\"ex:R\",\"ex:B\"]]]")
  (def oneOf "[\"SubClassOf\",\"ex:A\",[\"ObjectOneOf\",\"ex:R\",\"ex:R\",\"ex:B\"]]")

  (println (cs/parse-string subclassOf))

  (println (axiomTranslation/translate (cs/parse-string subclassOf)))
  (println (axiomTranslation/translate (cs/parse-string nesting)))
  (println (axiomTranslation/translate (cs/parse-string hasVaue)))
  (println (axiomTranslation/translate (cs/parse-string minCard)))
  (println (axiomTranslation/translate (cs/parse-string minQualifiedCard)))
  (println (axiomTranslation/translate (cs/parse-string maxCard)))
  (println (axiomTranslation/translate (cs/parse-string maxQualifiedCard)))
  (println (axiomTranslation/translate (cs/parse-string intersection)))
  (println (axiomTranslation/translate (cs/parse-string intersection2)))
  (println (axiomTranslation/translate (cs/parse-string union)))
  (println (axiomTranslation/translate (cs/parse-string oneOf)))
  (println (axiomTranslation/translate (cs/parse-string complemen)))
  (println (axiomTranslation/translate (cs/parse-string disjointClasses)))
  (println (axiomTranslation/translate (cs/parse-string disjointUnion)))
  (println (axiomTranslation/translate (cs/parse-string equivalentClasses))))
