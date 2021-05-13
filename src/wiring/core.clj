(ns wiring.core
  (:require [clojure.repl :as repl]
            [wiring.rdf2ofs :as rdf2ofs])
  (:gen-class))

(defn -main
  "Currently only used for manual testing."
  [& args]

  ;(println (cs/generate-string {:foo "bar" :baz 5}))
  (def testExistential "{\"rdf:type\":[{\"object\":\"owl:Restriction\"}],\"owl:onProperty\":[{\"object\":\"ex:part-of\"}],\"owl:someValuesFrom\":[{\"object\":\"ex:bar\"}]})")

  (def testNestingIntersection  "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:someValuesFrom\": [{\"object\": {\"owl:intersectionOf\": [{\"object\": {\"rdf:first\": [{\"object\": \"ex:B\"}], \"rdf:rest\": [{\"object\": {\"rdf:first\": [{\"object\": \"ex:C\"}], \"rdf:rest\": [{\"object\": \"rdf:nil\"}]}}]}}], \"rdf:type\": [{\"object\": \"owl:Class\"}]}}]}'}")
  (def testNestingRestrictions "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:someValuesFrom\": [{\"object\": {\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:someValuesFrom\": [{\"object\": \"ex:C\"}]}}]}")
  (def testExactCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:cardinality\": [{\"object\": \"2^^<xsd:nonNegativeInteger\"}]}")
  (def testExactQualifiedCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:qualifiedCardinality\": [{\"object\": \"2^^<xmls:nonNegativeInteger\"}], \"owl:onClass\": [{\"object\": \"ex:C\"}]}")
  (def testMinCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:minCardinality\": [{\"object\": \"2^^<xsd:nonNegativeInteger\"}]}")
  (def testMinQualifiedCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:minQualifiedCardinality\": [{\"object\": \"2^^<xmls:nonNegativeInteger\"}], \"owl:onClass\": [{\"object\": \"ex:B\"}]}")

  (println "Test Existential: ")
  (rdf2ofs/predicateMap2OFS testExistential)
  (println "")

  (println "Test Min Cardinality ")
  (rdf2ofs/predicateMap2OFS testMinCardinality)
  (println "")

  (println "Test Min Qualififed Cardinality ")
  (rdf2ofs/predicateMap2OFS testMinQualifiedCardinality)
  (println "")

  ;(println "Test Exact Cardinlaity ")
  ;(rdf2ofs/predicateMap2OFS testExactCardinality)
  ;(println "")

  (println "Test Nesting ")
  (rdf2ofs/predicateMap2OFS testNestingIntersection)
  (println "")

  (println "Test Nesting Restrictions")
  (rdf2ofs/predicateMap2OFS testNestingRestrictions)
  (println ""))
