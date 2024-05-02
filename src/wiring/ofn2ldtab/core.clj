(ns wiring.ofn2ldtab.core
  (:require [clojure.repl :as repl]
            [wiring.ofn2ldtab.axiomTranslation.translate :as t]
            [wiring.ofn2ldtab.axiomTranslation.classExpressionAxiom :as ct]
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
  (def subclassOf "[\"SubClassOf\",\"ex:A\",\"ex:B\"]")
  (def subclassOfAnnotated "[\"SubClassOf\",[\"Annotation\",[\"Annotation\",\"ex:annotationProperty2\",\"\\\"ex:annotationValue2\\\"^^string\"],\"ex:annotationProperty\",\"\\\"ex:annotationValue\\\"^^string\"],\"ex:A\",\"ex:B\"]")
  (def someValuesFrom "[\"SubClassOf\",\"ex:A\",[\"ObjectSomeValuesFrom\",\"ex:R\",\"ex:B\"]]")
  (def intersection "[\"SubClassOf\",\"ex:A\",[\"ObjectIntersectionOf\",\"ex:R\",\"ex:R\",\"ex:A\"]]")

  (def disjointUnion "[\"DisjointUnion\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")
  (def disjointClasses "[\"DisjointClasses\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")
  (def disjointClassesAnnotated "[\"DisjointClasses\",[\"Annotation\",\"ex:annotationProperty2\",\"\\\"ex:annotationValue2\\\"^^string\"],\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")
  (def disjointClasses2 "[\"DisjointClasses\",\"ex:A\",\"ex:D\"]")
  (def equivalentClasses "[\"EquivalentClasses\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")

  (def disjointProperties "[\"DisjointProperties\",\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")
  (def disjointPropertiesAnnotated "[\"DisjointProperties\",[\"Annotation\",\"ex:annotationProperty2\",\"\\\"ex:annotationValue2\\\"^^string\"],\"ex:A\",\"ex:D\",\"ex:C\",\"ex:R\",\"ex:B\"]")

  (def subPropertyOf "[\"SubObjectPropertyOf\",\"ex:Ap\",\"ex:Bp\"]")
  (def subPropertyOfAnnotated "[\"SubObjectPropertyOf\",[\"Annotation\",\"ex:annotationProperty2\",\"\\\"ex:annotationValue2\\\"^^string\"],\"ex:Ap\",\"ex:Bp\"]")

  (def domain "[\"ObjectPropertyDomain\",\"ex:Ap\",\"ex:Bp\"]")
  (def domainAnnotated "[\"ObjectPropertyDomain\",[\"Annotation\",\"ex:annotationProperty2\",\"\\\"ex:annotationValue2\\\"^^string\"],\"ex:Ap\",\"ex:Bp\"]")

  ;TODO test this
  (def subPropertyChainOf "[\"SubObjectPropertyOf\",[\"ObjectPropertyChain\",\"ex:Cp\",\"ex:Dp\"],\"ex:Bp\"]")
  (def subPropertyOfChainAnnotated "[\"SubObjectPropertyOf\",[\"Annotation\",\"ex:annotationProperty2\",\"\\\"ex:annotationValue2\\\"^^string\"],\"ex:Ap\",\"ex:Bp\"]")

  ;(def a (cs/generate-string (t/translate (cs/parse-string subclassOf) "graph")))
  ;(println a)
  ;(def a (cs/parse-string subclassOfAnnotated))
  (def a (cs/generate-string (t/translate (cs/parse-string subclassOfAnnotated) "graph")))
  ;(def aa (ct/has-annotation (cs/parse-string subclassOfAnnotated)))
  (println a)

  (println "")

  (def b (cs/generate-string (t/translate (cs/parse-string equivalentClasses) "graph")))
  (println b)

  (println "")

  (def c (cs/generate-string (t/translate (cs/parse-string disjointClasses) "graph")))

  (println c)
  (println "")

  (def c1 (cs/generate-string (t/translate (cs/parse-string disjointClassesAnnotated) "graph")))
  (println c1)

  (println "")

  (def d (cs/generate-string (t/translate (cs/parse-string disjointClasses2) "graph")))
  (println d)

  (println "")

  (def e (cs/generate-string (t/translate (cs/parse-string disjointProperties) "graph")))
  (println e)

  (def e1 (cs/generate-string (t/translate (cs/parse-string disjointPropertiesAnnotated) "graph"))) (println e1)

  (def f (cs/generate-string (t/translate (cs/parse-string subPropertyOf) "graph")))
  (println f)
  (def f1 (cs/generate-string (t/translate (cs/parse-string subPropertyOfAnnotated) "graph")))
  (println f1)

  (def g (cs/generate-string (t/translate (cs/parse-string domain) "graph")))
  (println g)

  (def g1 (cs/generate-string (t/translate (cs/parse-string domainAnnotated) "graph")))
  (println g1)

  (def h (cs/generate-string (t/translate (cs/parse-string subPropertyChainOf) "graph")))
  (println h)


  ;(println (u/get-datetype "\"asd\""))
  ;(println (u/get-datetype "<asd>")) 
  ;(println (u/get-datetype "asd:aaa"))
  ;(println (u/has-language-tag "\"asd\"@en"))
  ;(println (u/has-datatype "\"asd\"^^xsd:string"))
  ;(println (u/has-datatype "\"asd\"@en"))
  ;(println (u/has-language-tag "\"asd\"^^xsd:string"))
  )
