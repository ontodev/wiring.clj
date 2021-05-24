(ns wiring.core
  (:require [clojure.repl :as repl]
            [wiring.rdf2ofsObject :as rdf2ofsObject]
            [clojure.java.io :as io]
            [wiring.parsing :as p]
            [wiring.classTranslation :as cl]
            [wiring.axiomTranslation :as t]
            [wiring.spec :as spec])
  (:gen-class))

(defn -main
  "Currently only used for manual testing."
  [& args]

  ;(print (slurp (io/resource "tests/thickClassExpressions.txt")))

  (with-open [rdr (io/reader (io/resource "tests/thickClassExpressions.txt"))]
    (doseq [line (line-seq rdr)]

      (println (str "Input: " line))
      (println (str "Output: " (t/translate (p/parse line))))
      (println ""))))

(defn manualTesting []
  ;(println (cs/generate-string {:foo "bar" :baz 5}))
  (def testExistential "{\"rdf:type\":[{\"object\":\"owl:Restriction\"}],\"owl:onProperty\":[{\"object\":\"ex:part-of\"}],\"owl:someValuesFrom\":[{\"object\":\"ex:bar\"}]}")
  (def testNestingIntersection  "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:someValuesFrom\": [{\"object\": {\"owl:intersectionOf\": [{\"object\": {\"rdf:first\": [{\"object\": \"ex:B\"}], \"rdf:rest\": [{\"object\": {\"rdf:first\": [{\"object\": \"ex:C\"}], \"rdf:rest\": [{\"object\": \"rdf:nil\"}]}}]}}], \"rdf:type\": [{\"object\": \"owl:Class\"}]}}]}'}")
  (def testNestingRestrictions "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:someValuesFrom\": [{\"object\": {\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:someValuesFrom\": [{\"object\": \"ex:C\"}]}}]}")
  (def testExactCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:cardinality\": [{\"object\": \"2^^<xsd:nonNegativeInteger\"}]}")
  (def testExactQualifiedCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:qualifiedCardinality\": [{\"object\": \"2^^<xmls:nonNegativeInteger\"}], \"owl:onClass\": [{\"object\": \"ex:C\"}]}")
  (def testMinCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:minCardinality\": [{\"object\": \"2^^<xsd:nonNegativeInteger\"}]}")
  (def testMinQualifiedCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:minQualifiedCardinality\": [{\"object\": \"2^^<xmls:nonNegativeInteger\"}], \"owl:onClass\": [{\"object\": \"ex:B\"}]}")
  (def testInverse "{\"rdf:type\":[{\"object\":\"owl:Restriction\"}],\"owl:onProperty\":[{\"object\":{\"owl:inverseOf\":[{\"object\":\"ex:part-of\"}]}}],\"owl:someValuesFrom\":[{\"object\":\"ex:bar\"}]}")
  (def subclass "{\"subject\": \"ex:A\", \"predicate\": \"rdfs:subClassOf\", \"object\": {\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:cardinality\": [{\"object\": \"2^^<xmls:nonNegativeInteger\"}]}}")

  (def disjointclasses "{\"subject\": \"_:genid27\", \"predicate\": \"owl:AllDisjointClasses\", \"object\": {\"owl:members\": {\"rdf:first\": [{\"object\": \"ex:disjointClass1\"}], \"rdf:rest\": [{\"object\": {\"rdf:first\": [{\"object\": \"ex:disjointClass2\"}], \"rdf:rest\": [{\"object\": {\"rdf:first\": [{\"object\": \"ex:disjointClass3\"}], \"rdf:rest\": [{\"object\": \"rdf:nil\"}]}}]}}]}}}")
  (def equivalentClasses "{\"subject\": \"_:genid1\", \"predicate\": \"owl:equivalentClass\", \"object\": {\"rdf:rest\": [{\"object\": {\"rdf:rest\": [{\"object\": {\"rdf:rest\": [{\"object\": \"rdf:nil\"}], \"rdf:first\": [{\"object\": \"ex:equivalent3\"}]}}], \"rdf:first\": [{\"object\": \"ex:equivalent1\"}]}}], \"rdf:first\": [{\"object\": \"ex:equivalent2\"}]}}")

  (println "Test spec")
  (spec/tests testExistential)
  (println "")

  (println "Test Subclass ")
  (println (t/translate (p/parse subclass)))
  (println "")

  (println "Test disjoint classes ")
  (println (t/translate (p/parse disjointclasses)))
  (println "")

  (println "Test equivalent classes ")
  (println (t/translate (p/parse equivalentClasses)))
  (println "")

  (println "Test equivalent classes 2 ")
  (println (t/translate (p/parse equivalentClasses2)))
  (println "")

  (println "Test equivalent classes 3 ")
  (println (t/translate (p/parse maxCard)))
  (println "")

  (println "Test Nesting ")
  (println (cl/translate (p/parse testNestingIntersection)))
  (println ""))
