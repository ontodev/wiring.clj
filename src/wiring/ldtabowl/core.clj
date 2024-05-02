(ns wiring.ldtabowl.core
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [cheshire.core :as cs]
            [clojure.java.jdbc :as jdbc]
            [wiring.ldtab2ofn.parsing :as p]
            [wiring.typing.typeHandling :as typing]
            [wiring.ldtab2ofn.axiomTranslation.translate :as t]
            [wiring.ldtab2ofn.expressionTranslation.classTranslation :as ct]
            [wiring.ldtab2ofn.spec :as spec])
  (:gen-class))

(defn db-spec
  [path-to-database]
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname path-to-database})

(defn encode-json
  "Given information for a row in the statment table,
  encode thick-triple information as JSON strings."
  [transaction graph subject predicate object datatype annotation]
  {:assertion transaction
   :retraction 0 ;hard-coded value: data is being inserted
   :graph graph
   ;encode data as JSON strings
   :subject (if (string? subject) subject (cs/generate-string subject))
   :predicate predicate
   :object (if (string? object) object (cs/generate-string object))
   :datatype datatype
   :annotation (when annotation (cs/generate-string annotation))})

(defn insert-triples
  "Inserts a list of thick triples into a database."
  [json-triples db table]
  (jdbc/insert-multi! db (keyword table) (map #(encode-json (:assertion %)
                                                            (:graph %)
                                                            (:subject %)
                                                            (:predicate %)
                                                            (:object %)
                                                            (:datatype % ) (:annotation %)) json-triples)))


(defn get-complex-subsumptions
  [db]
  (jdbc/query db [(str "SELECT * FROM statement WHERE predicate='rdfs:subClassOf' AND datatype='_JSON'")]))

(defn get-complex-equivalences
  [db]
  (jdbc/query db [(str "SELECT * FROM statement WHERE predicate='owl:equivalentClass' AND datatype='_JSON'")]))

(defn get-types
  [db]
  (jdbc/query db [(str "SELECT * FROM statement WHERE predicate='rdf:type'")]))

(defn ldtab2owl
  [ldtab-sql types]
  (let [ldtab (p/parse-sql-thick-triple ldtab-sql)
        object (:object ldtab)
        owl (ct/translate object)
        owl-typed (typing/translate owl types)
        ldtab (assoc ldtab :object owl-typed)
        ldtab (assoc ldtab :datatype "_JSONOWL")]
    ldtab))

(defn -main
  "Currently only used for manual testing."
  [& args]

  (def db (db-spec (first args)))

  ;get type information
  (def types (typing/extractTyping (map #(t/translate (p/parse-sql-thick-triple %)) (get-types db))))

  ;insert _JSONOWL triples
  (insert-triples (map #(ldtab2owl % types) (get-complex-subsumptions db)) db "statement")
  (insert-triples (map #(ldtab2owl % types) (get-complex-equivalences db)) db "statement")

  ;delete old subsumption _JSON triples  
  (jdbc/delete! db :statement ["predicate = 'rdfs:subClassOf' AND datatype = '_JSON'"])
  (jdbc/delete! db :statement ["predicate = 'owl:equivalentClass' AND datatype = '_JSON'"])
  
  )
