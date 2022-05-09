(ns wiring.ldtab2ofn.axiomTranslation.core
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [cheshire.core :as cs] 
            [clojure.java.jdbc :as jdbc];THIS IS NOW A DEPENDENCY ON WIRING
            [wiring.ldtab2ofn.parsing :as p]
            [wiring.ldtab2ofn.axiomTranslation.translate :as t]
            [wiring.ldtab2ofn.spec :as spec])
  (:gen-class))

(defn get-triples
  [path-to-database]
  (let [db-spec {:classname "org.sqlite.JDBC"
            :subprotocol "sqlite"
            :subname path-to-database}] 
    (jdbc/query db-spec [(str "SELECT * FROM statement WHERE datatype='_JSON'")])))

(defn -main
  "Currently only used for manual testing."
  [& args]

  ;(with-open [rdr (io/reader (io/resource "tests/ldtab.txt"))]
  ;(with-open [rdr (io/reader (io/resource "tests/thickOBO.txt"))]
  ;(with-open [rdr (io/reader (io/resource "tests/thickClassExpressions.txt"))]
    ;(println (t/translate (first (p/get-triples-of "http://www.semanticweb.org/chris/ontologies/2022/1/untitled-ontology-354#A" (first args)))))))
    ;(println (p/get-triples-of "\"http://www.semanticweb.org/chris/ontologies/2022/1/untitled-ontology-354#A\"" (first args)))))
    ;(println (map t/translate (p/get-triples-of "\"http://www.semanticweb.org/chris/ontologies/2022/1/untitled-ontology-354#A\"" (first args))))))

    ;(println (map #(t/translate (p/parse-sql-thick-triple %)) (p/get-triples-of "\"http://www.semanticweb.org/chris/ontologies/2022/1/untitled-ontology-354#A\"" (first args))))))

    (run! println (map #(t/translate (p/parse-sql-thick-triple %)) (get-triples (first args)))))


    ;(println (t/translate (p/parse-sql-thick-triple (first (p/get-triples-of "\"http://www.semanticweb.org/chris/ontologies/2022/1/untitled-ontology-354#A\"" (first args))))))))
    ;(println (t/translate (p/parse-sql-thick-triple (second (p/get-triples-of "\"http://www.semanticweb.org/chris/ontologies/2022/1/untitled-ontology-354#LL\"" (first args))))))))

    ;(doseq [line (line-seq rdr)] 
    ;  (println (str "Input: " line))
    ;  (def ofn (cs/generate-string (t/translate (p/parse line))))
    ;  (println (str "Output: " ofn))
    ;  (println ""))))


;check datatypes for class expression axioms
;implement bits for annotations
;implement translation for thin triples

;DataHasValue has a literal (restriction)
;DataOneOf
