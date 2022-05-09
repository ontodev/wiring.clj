(ns wiring.ldtab2ofn.parsing
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]
            [clojure.spec.alpha :as spec] 
            [clojure.java.jdbc :as jdbc];THIS IS NOW A DEPENDENCY ON WIRING
            [wiring.ldtab2ofn.spec :as owlspec]))

(defn handleNamespaces
  "Replace standard namespace IRI's with their name prefixes"
  [input]
  (let [owl (s/replace input #"http://www.w3.org/2002/07/owl#" "owl:")
        rdf (s/replace owl #"http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf:")
        rdfs (s/replace rdf #"http://www.w3.org/2000/01/rdf-schema#" "rdfs:")
        xsd (s/replace rdfs #"http://www.w3.org/2001/XMLSchema#" "xsd:")]
    xsd))

;TODO DELETE THIS
(defn parse
  "Parse JSON predicate map."
  [predicateMap]
  {:pre [(spec/valid? string? predicateMap)] ;a 'map' can be just a string, i.e. a normal object of a triple
   :post [(spec/valid? ::owlspec/map %)]}
  (let [removedObject (s/replace predicateMap #"\[\{\"object\":" "") ;TODO that doesn't work no more...
        removedClosingBracket (s/replace removedObject  #"\}\]" "")
        namespacePrefixes (handleNamespaces removedClosingBracket) 
        predicates (cs/parse-string namespacePrefixes  true)]
    predicates))

(defn load-db
  [path]
  {:classname "org.sqlite.JDBC"
  :subprotocol "sqlite"
  :subname path})

(defn ldtab-json-parse
  [string]
  (try (cs/parse-string string true)
       (catch Exception e string)))


(defn parse-sql-thick-triple
  [thick] 
  (-> thick
    (update :subject #(ldtab-json-parse %))
    (update :predicate #(ldtab-json-parse %))
    (update :object #(ldtab-json-parse %))
    (update :datatype #(ldtab-json-parse %))
    (update :annotation #(ldtab-json-parse %))))



(defn get-triples-of
  [subject path-to-database]
  (def db-spec (load-db path-to-database))
  ;(println (jdbc/query db-spec ["SELECT * FROM prefix"])))
  (jdbc/query db-spec [(str "SELECT * FROM statement WHERE subject='" subject "'")]))


