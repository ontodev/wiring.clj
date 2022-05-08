(ns wiring.ofn2ldtab.util
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]))


(defn get-datetype
  [input]
  (cond 
    (coll? input) "JSON"
    (re-matches #"^\"(.+)\"(.*)$" input) "LITERAL"
    (re-matches #"^<(.+)>$" input) "URI"
    (re-matches #"^(.+):(.+)$" input) "CURIE"))

(defn has-datatype
  [literal]
  (last (re-matches #"^\"(.+)\"\^\^(.*)$" literal)))

(defn has-language-tag
  [literal]
  (last (re-matches #"^\"(.+)\"@(.*)$" literal)))

(defn get-curie-namespace
  [curie]
  (first (re-matches #"^(.+):(.+)$" curie)))

(defn get-curie-localname
  [curie]
  (second (re-matches #"^(.+):(.+)$" curie)))

(defn translate-literal
  [literal]
  (let [language-tag (has-language-tag literal)
        datatype (has-datatype literal)]
    (cond
      language-tag (str "@" language-tag)
      datatype datatype
      :else "_plain")))


(defn translate-datatype
  [string]
  (let [datatype (get-datetype string)]
    (case datatype
      "JSON" "_JSON"
      "LITERAL" (translate-literal string)
      "URI" "_IRI"
      "CURIE" "_IRI"
      :else "ERROR")))


;TODO
(defn jsonFormat
  "Serialises a list of entities into a valid OFN-S expression."
  [& args]
  (let [s (seq args)
        firstOperator (str "\"" (first s) "\"")
        arguments (interpose, "," (rest s))
        string (apply str arguments)
        brackets (str "[" firstOperator "," string "]")]
    brackets))
