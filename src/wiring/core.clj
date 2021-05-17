(ns wiring.core
  (:require [clojure.repl :as repl]
            [wiring.rdf2ofsObject :as rdf2ofsObject]
            [wiring.parsing :as p]
            [wiring.classTranslation :as cl]
            [wiring.axiomTranslation :as t]
            [wiring.spec :as spec])
  (:gen-class))

(defn -main
  "Currently only used for manual testing."
  [& args]

  ;(println (cs/generate-string {:foo "bar" :baz 5}))
  (def testExistential "{\"rdf:type\":[{\"object\":\"owl:Restriction\"}],\"owl:onProperty\":[{\"object\":\"ex:part-of\"}],\"owl:someValuesFrom\":[{\"object\":\"ex:bar\"}]}")

  (def testNestingIntersection  "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:someValuesFrom\": [{\"object\": {\"owl:intersectionOf\": [{\"object\": {\"rdf:first\": [{\"object\": \"ex:B\"}], \"rdf:rest\": [{\"object\": {\"rdf:first\": [{\"object\": \"ex:C\"}], \"rdf:rest\": [{\"object\": \"rdf:nil\"}]}}]}}], \"rdf:type\": [{\"object\": \"owl:Class\"}]}}]}'}")
  (def testNestingRestrictions "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:someValuesFrom\": [{\"object\": {\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:someValuesFrom\": [{\"object\": \"ex:C\"}]}}]}")
  (def testExactCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:cardinality\": [{\"object\": \"2^^<xsd:nonNegativeInteger\"}]}")
  (def testExactQualifiedCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:qualifiedCardinality\": [{\"object\": \"2^^<xmls:nonNegativeInteger\"}], \"owl:onClass\": [{\"object\": \"ex:C\"}]}")
  (def testMinCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:minCardinality\": [{\"object\": \"2^^<xsd:nonNegativeInteger\"}]}")
  (def testMinQualifiedCardinality "{\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:minQualifiedCardinality\": [{\"object\": \"2^^<xmls:nonNegativeInteger\"}], \"owl:onClass\": [{\"object\": \"ex:B\"}]}")

  ;;this is created MANUALLY
  (def testInverse "{\"rdf:type\":[{\"object\":\"owl:Restriction\"}],\"owl:onProperty\":[{\"object\":{\"owl:inverseOf\":[{\"object\":\"ex:part-of\"}]}}],\"owl:someValuesFrom\":[{\"object\":\"ex:bar\"}]}")


  (def subclass "{\"subject\": \"ex:A\", \"predicate\": \"rdfs:subClassOf\", \"object\": {\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:prop\"}], \"owl:cardinality\": [{\"object\": \"2^^<xmls:nonNegativeInteger\"}]}}")
  (def disjointclasses "{\"subject\": \"_:genid4\", \"predicate\": \"owl:members\", \"object\": {\"rdf:first\": [{\"object\": \"ex:A\"}], \"rdf:rest\": [{\"object\": {\"rdf:first\": [{\"object\": \"ex:D\"}], \"rdf:rest\": [{\"object\": {\"rdf:first\": [{\"object\": \"ex:E\"}], \"rdf:rest\": [{\"object\": {\"rdf:first\": [{\"object\": \"ex:F\"}], \"rdf:rest\": [{\"object\": \"rdf:nil\"}]}}]}}]}}]}}")


  ;"{"rdf:type": [{"object": "owl:Restriction"}], "owl:onProperty": [{"object": "ex:prop"}], "owl:someValuesFrom": [{"object": {"rdf:type": [{"object": "owl:Restriction"}], "owl:onProperty": [{"object": "ex:prop"}], "owl:someValuesFrom": [{"object": "ex:C"}]}}]}

  ;{"rdf:type":[{"object":"owl:Restriction"}],"owl:onProperty":[{"object": {"owl:inverseOf":[{"object":"ex:part-of"}]}}],"owl:someValuesFrom":[{"object":"ex:bar"}]}

  (println "Test Existential: ")
  (rdf2ofsObject/predicateMap2OFS testExistential)
  (println "")

  (println "Test Min Cardinality ")
  (rdf2ofsObject/predicateMap2OFS testMinCardinality)
  (println "")

  (println "Test Min Qualififed Cardinality ")
  (rdf2ofsObject/predicateMap2OFS testMinQualifiedCardinality)
  (println "")

  (println "Test Nesting ")
  (rdf2ofsObject/predicateMap2OFS testNestingIntersection)
  (println "")

  (println "Test Nesting Restrictions")
  (rdf2ofsObject/predicateMap2OFS testNestingRestrictions)
  (println "")

  (println "Test spec")
  (spec/tests testExistential)
  (println "")

  (println "Test Subclass ")
  (println (t/translate (p/parse subclass)))
  (println "")

  (println "Test disjoint classes ")
  (println (t/translate (p/parse disjointclasses)))
  (println "")

  (println "Test Nesting ")
  (println (cl/translate (p/parse testNestingIntersection)))
  (println "") 
  )
