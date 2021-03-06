(ns wiring.core 
  (:require [clojure.test :refer :all] 
            [wiring.thick2ofn.parsing :as p]
            [cheshire.core :as cs]
            [clojure.java.io :as io]
            [wiring.thick2ofn.axiomTranslation.translate :as thick2ofn]
            [wiring.ofn2thick.axiomTranslation :as ofn2thick]))

(defn thick-ofn-thick-round-trip
  "Checks round-trip translation from
  'thick triples' to
  'OFN-S expression' back to
  'thick tripels'"
  [inputString] 
    (def thick2ofnTranslation (cs/generate-string (thick2ofn/translate (p/parse inputString))))
    (def ofn2thickTranslation (cs/generate-string (ofn2thick/translate (cs/parse-string thick2ofnTranslation))))
    (= (p/parse inputString) (p/parse ofn2thickTranslation)))

;unit test for existential restriction
(deftest someValuesFrom
  (testing "someValuesFrom"
    (def inputString "{\"subject\": \"ex:existential\", \"predicate\": \"rdfs:subClassOf\", \"object\": {\"rdf:type\": [{\"object\": \"owl:Restriction\"}], \"owl:onProperty\": [{\"object\": \"ex:pExistential\"}], \"owl:someValuesFrom\": [{\"object\": \"ex:existentialFiller\"}]}}")
    (is (thick-ofn-thick-round-trip inputString))))

;NB: the input test file does not include 'thin triples'
;and does not include corner cases that involve the use of blank nodes
(deftest prototype
  (testing "Thick triples from prototype.py"
    (with-open [rdr (io/reader (io/resource "tests/roundTripInput.txt"))]
      (doseq [line (line-seq rdr)]
        (is (thick-ofn-thick-round-trip line)))))) 
