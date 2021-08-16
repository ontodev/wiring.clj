(ns wiring.ofn2thick.axiomTranslation.core
  (:require [clojure.repl :as repl]
            [wiring.ofn2thick.axiomTranslation.translate :as axiomTranslation]
            [wiring.thick2ofn.axiomTranslation.translate :as t]
            [wiring.thick2ofn.parsing :as p]
            [cheshire.core :as cs]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main
  "Currently only used for manual testing."
  [& args] 
    (with-open [rdr (io/reader (io/resource "tests/thickOBO.txt"))]
      (doseq [line (line-seq rdr)]
        ;(println "Input: " line)
        ;(println "OFN: " (cs/generate-string (t/translate (cs/parse-string line true))))
        (def ofn (cs/generate-string (t/translate (p/parse line))))
        ;(println "OFN: " ofn)
        (def orig (axiomTranslation/translate (cs/parse-string ofn)))
        ;(println "Orig: " orig)

        (if (= (cs/parse-string line true) orig)
          (print "")
          (println line "\n" ofn "\n" orig))



        )))
        ;(is (thick-ofn-thick-round-trip line)))))

  ;(def subclassOf "[\"SubClassOf\",\"ex:A\",\"ex:B\"]")
  ;;(def subclassOf "[\"SubClassOf\",\"ex:A\",[\"ObjectSomeValuesFrom\",\"ex:R\",\"ex:B\"]]")
  ;(def disjointUnion "[\"DisjointUnion\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")
  ;(def disjointClasses "[\"DisjointClasses\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")
  ;(def equivalentClasses "[\"EquivalentClasses\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")

  ;(def hasVaue "[\"SubClassOf\",\"ex:A\",[\"ObjectHasValue\",\"ex:R\",\"ex:B\"]]")
  ;(def minCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMinCardinality\",\"1\",\"ex:R\"]]")
  ;(def minQualifiedCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMinCardinality\",\"1\",\"ex:R\",\"ex:A\"]]")
  ;(def maxCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMaxCardinality\",\"1\",\"ex:R\"]]")
  ;(def maxQualifiedCard "[\"SubClassOf\",\"ex:A\",[\"ObjectMaxCardinality\",\"1\",\"ex:R\",\"ex:A\"]]")
  ;(def intersection "[\"SubClassOf\",\"ex:A\",[\"ObjectIntersectionOf\",\"1\",\"ex:R\",\"ex:A\"]]")
  ;(def union "[\"SubClassOf\",\"ex:A\",[\"ObjectUnionOf\",\"1\",\"ex:R\",\"ex:A\"]]")
  ;(def complemen "[\"SubClassOf\",\"ex:A\",[\"ObjectComplementOf\",\"ex:A\"]]")
  ;(def inverse "[\"SubClassOf\",\"ex:A\",[\"ObjectSomeValuesFrom\",[\"ObjectInverseOf\",\"ex:R\"],[\"ObjectSomeValuesFrom\",\"ex:R\",\"ex:B\"]]]") 

  ;(println maxQualifiedCard)
  ;(def a (cs/generate-string (axiomTranslation/translate (cs/parse-string maxQualifiedCard))))
  ;(println (cs/generate-string (t/translate (p/parse a)))) 

  ;(println "") 
  ;(def thin "[\"ThinTriple\",\"s\",\"p\",\"o\"]") 
  ;(println (axiomTranslation/translate (cs/parse-string thin))) 
  ;(def ts {:subject "s" :predicate "p" :object "o"})
  ;(def aa (cs/generate-string ts))
  ;(println aa) 

;  )
