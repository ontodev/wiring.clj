(ns wiring.ofn2rdfa.propertyTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io])
  (:gen-class))

;TODO data validation
(declare translate)

(defn namedProperty?
  "Checks whether a property is a named property"
  [ofn]
  (let [operator (first ofn)];
    (case operator
      "ObjectInverseOf" false
      true)))

(defn labelSubstitution
  "Translate a (named) class expression"
  [namedProperty subject2label]
  (if (contains? subject2label namedProperty)
    (first (get subject2label namedProperty))
    namedProperty))

(defn baseTranslation 
  "Translate a (named) class expression"
  ([namedProperty subject2label] ;no property given
  (let [href "<a href"
        entity (str href "=" \" namedProperty \")
        prop (str entity ">")
        label (str prop (labelSubstitution namedProperty subject2label))
        closing (str label "</a>")]
    closing))

  ([namedProperty subject2label property]
  (let [href "<a href"
        entity (str href "=" \" namedProperty \")
        prop (str entity " property=\"" property \" ">")
        label (str prop (labelSubstitution namedProperty subject2label))
        closing (str label "</a>")]
    closing))) 

(defn spanOpening; this will only be called for the inverse of operator
  "Determines the opening span for an RDFa serialistion."  
  ([input]
    (str "<span typeof=\"owl:ObjectProperty\">"))
  ([input property];RDFa property argument from parent call
     (str "<span property=" \" property \" " typeof=\"owl:ObjectProperty\">")))

(defn translateInverseOf
  "Translate ObjectInverseOf expression"
  ([ofn subject2label]
  (let [[op arg] ofn
    htmlOpen "" ;this is a compound expression - so it needs to be put in parenthesis
    opening (str htmlOpen (spanOpening ofn))
    inverse (str opening " inverse (")
    prop (str inverse " " (translate arg subject2label "owl:inverseOf"))
    closing (str prop " </span>")
    htmlClosing (str closing ")")]
   htmlClosing )) 
  ([ofn subject2label propertyRDFa]
  (let [[op arg] ofn
    htmlOpen "" ;this is a compound expression - so it needs to be put in parenthesis
    opening (str htmlOpen (spanOpening ofn propertyRDFa))
    inverse (str opening " inverse (")
    prop (str inverse " " (translate arg subject2label "owl:inverseOf"))
    closing (str prop " </span>")
    htmlClosing (str closing ")")]
   htmlClosing)))


(defn translate
  "Translate OFN-S property expression into predicate map"
  ([ofn subject2label]
  (if (= "ObjectInverseOf" (first ofn))
    (translateInverseOf ofn subject2label)
    (baseTranslation ofn subject2label)) )
  ([ofn subject2label propertyRDFa]
  (if (= "ObjectInverseOf" (first ofn))
    (translateInverseOf ofn subject2label propertyRDFa)
    (baseTranslation ofn subject2label propertyRDFa))))
