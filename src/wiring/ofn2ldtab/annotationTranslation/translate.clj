(ns wiring.ofn2ldtab.annotationTranslation.translate
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [wiring.ofn2ldtab.util :as u]
            [clojure.spec.alpha :as spec]
            [wiring.ofn2ldtab.spec :as owlspec])

  (:gen-class))
(declare translate)

(defn is-literal
  [input]
  (let [match (re-matches #"^\"(.+)\"(.*)$" input)]
    (if match 
      (second match)
      nil)))

(defn has-datatype
  [literal]
  (last (re-matches #"^\"(.+)\"\^\^(.*)$" literal)))

(defn has-language-tag
  [literal]
  (last (re-matches #"^\"(.+)\"@(.*)$" literal)))

(defn encode-annotation-value
  [value]
  (let [literal (is-literal value) 
        datatype (has-datatype value)
        language (has-language-tag value)]
    (cond
      ;TODO we ignore anonymous individuals here (but they would be typed as "_JSON" unless we skolemise them)
      (not literal) 
      {"object" value
       "datatype" "_IRI"
       "meta" "Annotation"}
      (and literal datatype)
      {"object" literal
       "datatype" datatype
       "meta" "Annotation"}
      (and literal language)
      {"object" literal
       "datatype" (str "@" language)
       "meta" "Annotation"}
      :else 
      {"object" literal
       "datatype" "_plain"
       "meta" "Annotation"})))

(defn translate
  "Translate OFN-S expression to thick triple"
  [ofn]
  (cond
    (= 3 (count ofn))
    (let [[_op property value] ofn
          v (encode-annotation-value value)]
      {property [v]})
    (= 4 (count ofn)) ;annotation in an annotation
    (let [[_op annotation property value] ofn
          nested-ann (translate annotation)
          v (encode-annotation-value value)]
      {property [v]
       "annotation" nested-ann})
    :else;
    "NULL"));TODO set to NULL or leave empty?
