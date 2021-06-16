(ns wiring.ofn2rdfa.classTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [wiring.ofn2rdfa.propertyTranslation :as property])
            ;[wiring.thick2ofn.spec :as spec])
  (:gen-class))

;TODO data validation
(declare translate)

(defn namedClass?
  "Checks whether an expression is a named class."
  [ofn]
  (let [operator (first ofn)];
    (case operator
      "ObjectSomeValuesFrom" false
      "ObjectAllValuesFrom" false
      "ObjectHasValue"  false
      "ObjectMinCardinality" false
      "ObjectMaxCardinality" false
      "ObjectExactCardinality" false
      "ObjectIntersectionOf" false
      "ObjectUnionOf" false
      "ObjectOneOf" false
      "ObjectComplementOf" false
      "ObjectHasSelf" false
      true)))

;determine type first
(defn getType
  "Determines the rdf:type of a class expression"
  [ofn]
  (let [operator (first ofn)]
    (case operator 
      "ObjectSomeValuesFrom" "owl:Restriction"
      "ObjectAllValuesFrom" "owl:Restriction"
      "ObjectHasValue" "owl:Restriction"
      "ObjectMinCardinality" "owl:Restriction"
      "ObjectMaxCardinality" "owl:Restriction"
      "ObjectExactCardinality" "owl:Restriction"
      "ObjectHasSelf" "owl:Restriction"
      "ObjectIntersectionOf" "owl:Class"
      "ObjectUnionOf" "owl:Class"
      "ObjectOneOf" "owl:Class"
      "ObjectComplementOf" "owl:Class"
      ofn)));return input as named class


(defn labelSubstitution
  "Translate a (named) class expression"
  [namedClass subject2label]
  (if (contains? subject2label namedClass)
    (first (get subject2label namedClass))
    namedClass))

(defn baseTranslation 
  "Translate a (named) class expression"
  ([namedClass subject2label] ;no property given
  (let [href "<a href"
        entity (str href "=" \" namedClass \")
        prop (str entity ">")
        label (str prop (labelSubstitution namedClass subject2label))
        closing (str label "</a>")]
    closing))

  ([namedClass subject2label property]
  (let [href "<a href"
        entity (str href "=" \" namedClass \")
        prop (str entity " property=\"" property \" ">")
        label (str prop (labelSubstitution namedClass subject2label))
        closing (str label "</a>")]
    closing)))

(defn spanOpening
  "Determines the opening span for an RDFa serialistion."  
  ([input]
  (if (namedClass? input)
    (str "<span about=" \" input \" ">")
    (str "<span typeof=" \"  (getType input) \" ">")))
  ([input property];RDFa property argument from parent call
   (if (namedClass? input)
     (str "<span property=" \" property \" " about=" \" input \" ">")
     (str "<span property=" \" property \" " typeof=" \"  (getType input) \" ">"))))


(defn renderRestriction 
  "Renders a class restriction" 
  [span property modifer filler]
  (let [htmlOpen "(" ;this is a compound expression - so it needs to be put in parenthesis
       opening (str htmlOpen span)
       prop (str opening " " property)
       via (str prop modifer)
       fill (str via filler)
       closing (str fill " </span>")
       htmlClosing (str closing ")")]
    htmlClosing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      RDF lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO: there are ways to write lists in RDFa much more succintly.
;unfortunately, I couldn't get more succinct notations working in nested epxressions.

(defn translateList
  "Translate class expressions into an RDF list"
  [expressions subject2label htmlMarkup]
  (loop [in (rest (reverse expressions));constructing list from last element to first
         out (str "<span property=\"rdf:rest\" typeof=\"owl:Class\"> "
                 htmlMarkup " " 
                 (translate (first (reverse expressions)) subject2label "rdf:first")
                  " <span resource=\"rdf:nil\" property=\"rdf:rest\"></span>"
                  "</span>")]
    (if (empty? in)
      out
      (recur (rest in)
             (if (empty? (rest in))
               (str (translate (first in) subject2label "rdf:first") " " out) 
               (str "<span property=\"rdf:rest\" typeof=\"owl:Class\"> "
                htmlMarkup " " (translate (first in) subject2label "rdf:first") " " out
               " </span>"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Restrictions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn translateObjectSomeValuesFrom
  "Translate a ObjectSomeValuesFrom expression"
  ([ofn subject2label]
  (let [[op property filler] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer " some "
     fill (translate filler subject2label "owl:someValuesFrom")]
     (renderRestriction opening prop modifer fill))) 

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op property filler] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer " some "
     fill (translate filler subject2label "owl:someValuesFrom")]
     (renderRestriction opening prop modifer fill))))

(defn translateObjectAllValuesFrom
  "Translate a ObjectAllValuesFrom expression"
  ([ofn subject2label]
  (let [[op property filler] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer " only "
     fill (translate filler subject2label "owl:allValuesFrom")]
     (renderRestriction opening prop modifer fill))) 

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op property filler] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer " only "
     fill (translate filler subject2label "owl:allValuesFrom")]
     (renderRestriction opening prop modifer fill))))
   
(defn translateObjectMinUnqualifiedCardinality
  "Translate a ObjectMinCardinality expression"
  ([ofn subject2label]
  (let [[op cardinality property] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " min ")
     card (str "<span property=" \" "owl:minCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")]
     ;fill (translate filler subject2label "owl:minCardinality")]
     (renderRestriction opening prop modifer card)))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " min ")
     card (str "<span property=" \" "owl:minCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")]
     ;fill (translate filler subject2label "owl:allValuesFrom")]
     (renderRestriction opening prop modifer card))))

(defn translateObjectMinQualifiedCardinality 
  "Translate a ObjectMinCardinality expression" 
  ([ofn subject2label]
  (let [[op cardinality property filler] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " min ")
     fill (translate filler subject2label "owl:onClass")
     card (str "<span property=" \" "owl:minQualifiedCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")]
     (renderRestriction opening prop modifer (str card " " fill))))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property filler] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " min ")
     card (str "<span property=" \" "owl:minQualifiedCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")
     fill (translate filler subject2label "owl:onClass")]
     (renderRestriction opening prop modifer (str card " " fill))))) 

(defn translateObjectMinCardinality
  "Translate a ObjectMinCardinality expression"
  ([ofn subject2label]
  (if (= 3 (count ofn))
    (translateObjectMinUnqualifiedCardinality ofn subject2label)
    (translateObjectMinQualifiedCardinality ofn subject2label)))
  ([ofn subject2label property]
  (if (= 3 (count ofn))
    (translateObjectMinUnqualifiedCardinality ofn subject2label property)
    (translateObjectMinQualifiedCardinality ofn subject2label property))))

(defn translateObjectMaxUnqualifiedCardinality
  "Translate a ObjectMaxCardinality expression"
  ([ofn subject2label]
  (let [[op cardinality property] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " max " cardinality)
     card (str "<span property=" \" "owl:maxCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")]
     ;fill (translate filler subject2label "owl:minCardinality")]
     (renderRestriction opening prop modifer card)))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " max ")
     card (str "<span property=" \" "owl:maxCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")]
     ;fill (translate filler subject2label "owl:allValuesFrom")]
     (renderRestriction opening prop modifer card))))

(defn translateObjectMaxQualifiedCardinality 
  "Translate a ObjectMaxCardinality expression" 
  ([ofn subject2label]
  (let [[op cardinality property filler] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " max ")
     fill (translate filler subject2label "owl:onClass")
     card (str "<span property=" \" "owl:maxQualifiedCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")]
     (renderRestriction opening prop modifer (str card " " fill))))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property filler] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " max ")
     card (str "<span property=" \" "owl:maxQualifiedCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")
     fill (translate filler subject2label "owl:onClass")]
     (renderRestriction opening prop modifer (str card " " fill))))) 

(defn translateObjectMaxCardinality
  "Translate a ObjectMaxCardinality expression"
  ([ofn subject2label]
  (if (= 3 (count ofn))
    (translateObjectMaxUnqualifiedCardinality ofn subject2label)
    (translateObjectMaxQualifiedCardinality ofn subject2label)))
  ([ofn subject2label property]
  (if (= 3 (count ofn))
    (translateObjectMaxUnqualifiedCardinality ofn subject2label property)
    (translateObjectMaxQualifiedCardinality ofn subject2label property))))

(defn translateObjectExactUnqualifiedCardinality
  "Translate a ObjectExactCardinality expression"
  ([ofn subject2label]
  (let [[op cardinality property] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " exactly " cardinality)
     card (str "<span property=" \" "owl:cardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")]
     ;fill (translate filler subject2label "owl:minCardinality")]
     (renderRestriction opening prop modifer card)))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " exactly ")
     card (str "<span property=" \" "owl:cardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")]
     ;fill (translate filler subject2label "owl:allValuesFrom")]
     (renderRestriction opening prop modifer card))))

(defn translateObjectExactQualifiedCardinality 
  "Translate a ObjectExactCardinality expression" 
  ([ofn subject2label]
  (let [[op cardinality property filler] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " exactly ")
     fill (translate filler subject2label "owl:onClass")
     card (str "<span property=" \" "owl:qualifiedCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")]
     (renderRestriction opening prop modifer (str card " " fill))))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property filler] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " exactly ")
     card (str "<span property=" \" "owl:qualifiedCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")
     fill (translate filler subject2label "owl:onClass")]
     (renderRestriction opening prop modifer (str card " " fill))))) 

(defn translateObjectExactCardinality
  "Translate a ObjectExactCardinality expression"
  ([ofn subject2label]
  (if (= 3 (count ofn))
    (translateObjectExactUnqualifiedCardinality ofn subject2label)
    (translateObjectExactQualifiedCardinality ofn subject2label)))
  ([ofn subject2label property]
  (if (= 3 (count ofn))
    (translateObjectExactUnqualifiedCardinality ofn subject2label property)
    (translateObjectExactQualifiedCardinality ofn subject2label property))))

(defn translateObjectHasValue
  "Translate a ObjectHasValue expression"
  ([ofn subject2label]
  (let [[op property filler] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer " value "
     fill (translate filler subject2label "owl:hasValue")]
     (renderRestriction opening prop modifer fill))) 

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op property filler] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer " value "
     fill (translate filler subject2label "owl:hasValue")]
     (renderRestriction opening prop modifer fill))))

(defn translateObjectHasSelf
  "Translate a ObjectHasValue expression"
  ([ofn subject2label]
  (let [[op property] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer " some Self "
     fill (str "<span property=\"owl:hasSelf\" hidden="true">\"true\"^^xsd:boolean</span>")]
     (renderRestriction opening prop modifer fill))) 

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op property] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer " some Self "
     fill (str "<span property=\"owl:hasSelf\" hidden="true">\"true\"^^xsd:boolean</span>")] 
     (renderRestriction opening prop modifer fill))))

(defn translateObjectIntersection
  "Translate an ObjectIntersectionOf expression"
  ([ofn subject2label]
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn) 
        intersectionClass (str htmlOpening  "(<span property=\"owl:intersectionOf\" typeof=\"owl:Class\"> ")
        operands (str intersectionClass (translateList arguments subject2label "and"))
        intersectionClosing  (str operands "</span>)")
        htmlClosing (str intersectionClosing "</span>")]
    htmlClosing)) 
  ([ofn subject2label propertyRDFa]
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn propertyRDFa)
        intersectionClass (str htmlOpening  "(<span property=\"owl:intersectionOf\" typeof=\"owl:Class\"> ")
        operands (str intersectionClass (translateList arguments subject2label "and"))
        intersectionClosing  (str operands "</span>)")
        htmlClosing (str intersectionClosing  "</span>")]
    htmlClosing))) 

(defn translateObjectUnion
  "Translate an ObjectUnionOf expression"
  ([ofn subject2label]
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn) 
        unionClass (str htmlOpening  "(<span property=\"owl:unionOf\" typeof=\"owl:Class\"> ")
        operands (str unionClass (translateList arguments subject2label "or"))
        unionClosing  (str operands "</span>)")
        htmlClosing (str unionClosing "</span>")]
    htmlClosing)) 
  ([ofn subject2label propertyRDFa]
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn propertyRDFa)
        unionClass (str htmlOpening  "(<span property=\"owl:unionOf\" typeof=\"owl:Class\"> ")
        operands (str unionClass (translateList arguments subject2label "or"))
        unionClosing  (str operands "</span>)")
        htmlClosing (str unionClosing  "</span>")]
    htmlClosing))) 

(defn translateObjectOneOf
  "Translate an ObjectOneOf expression"
([ofn subject2label] 
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn) 
        oneOf (str htmlOpening  "<span property=\"owl:oneOf\" typeof=\"owl:Class\"> ")
        operands (str oneOf (translateList arguments subject2label ""))
        unionClosing  (str operands "</span>")
        htmlClosing (str unionClosing "</span>")]
    htmlClosing)) 
  ([ofn subject2label propertyRDFa]
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn propertyRDFa)
        oneOf (str htmlOpening  "<span property=\"owl:oneOf\" typeof=\"owl:Class\"> ")
        operands (str oneOf (translateList arguments subject2label ""))
        unionClosing  (str operands "</span>")
        htmlClosing (str unionClosing  "</span>")]
    htmlClosing))) 

(defn translateObjectComplement
  "Translate an ObjectComplementOf expression"
  ([ofn subject2label]
  (let [[operator argument] ofn
    htmlOpen "" ;this is a compound expression - so it needs to be put in parenthesis
    opening (str htmlOpen (spanOpening ofn))
    inverse (str opening " not (")
    prop (str inverse " " (translate argument subject2label "owl:complementOf"))
    closing (str prop " </span>")
    htmlClosing (str closing ")")]
   htmlClosing )) 
  ([ofn subject2label propertyRDFa]
  (let [[operator argument] ofn
    htmlOpen "" ;this is a compound expression - so it needs to be put in parenthesis
    opening (str htmlOpen (spanOpening ofn propertyRDFa))
    inverse (str opening " not (")
    prop (str inverse " " (translate argument subject2label "owl:complementOf"))
    closing (str prop " </span>")
    htmlClosing (str closing ")")]
   htmlClosing ))) 




(defn translate
  "Translate OFN-S expression to manchester synax"
  ([ofn subject2label]
  (let [operator (first ofn)];
    (case operator
      "ObjectSomeValuesFrom"  (translateObjectSomeValuesFrom ofn subject2label)
      "ObjectAllValuesFrom"  (translateObjectAllValuesFrom ofn subject2label)
      "ObjectMinCardinality"  (translateObjectMinCardinality ofn subject2label)
      "ObjectMaxCardinality"  (translateObjectMaxCardinality ofn subject2label)
      "ObjectExactCardinality"  (translateObjectExactCardinality ofn subject2label)
      "ObjectHasValue"  (translateObjectHasValue ofn subject2label)
      "ObjectIntersectionOf"  (translateObjectIntersection ofn subject2label)
      "ObjectUnionOf"  (translateObjectUnion ofn subject2label)
      "ObjectOneOf"  (translateObjectOneOf ofn subject2label)
      "ObjectHasSelf"  (translateObjectHasSelf ofn subject2label)
      "ObjectComplementOf"  (translateObjectComplement ofn subject2label)
      (baseTranslation ofn subject2label))))
  ([ofn subject2label property]
   (let [operator (first ofn)]
     (case operator
      "ObjectSomeValuesFrom" (translateObjectSomeValuesFrom ofn subject2label property)
      "ObjectAllValuesFrom" (translateObjectAllValuesFrom ofn subject2label property)
      "ObjectMinCardinality"  (translateObjectMinCardinality ofn subject2label property)
      "ObjectMaxCardinality"  (translateObjectMaxCardinality ofn subject2label property)
      "ObjectExactCardinality"  (translateObjectExactCardinality ofn subject2label property)
      "ObjectHasValue"  (translateObjectHasValue ofn subject2label property)
      "ObjectIntersectionOf"  (translateObjectIntersection ofn subject2label property) 
      "ObjectUnionOf"  (translateObjectUnion ofn subject2label property) 
      "ObjectOneOf"  (translateObjectOneOf ofn subject2label property) 
      "ObjectHasSelf" (translateObjectHasSelf ofn subject2label property)
      "ObjectComplementOf"  (translateObjectComplement ofn subject2label property)
      (baseTranslation ofn subject2label property)))))


