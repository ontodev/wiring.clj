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
     modifer (str " min " cardinality)
     card (str "<span property=" \" "owl:minCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")]
     ;fill (translate filler subject2label "owl:minCardinality")]
     (renderRestriction opening prop modifer card)))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " min " cardinality)
     card (str "<span property=" \" "owl:minCardinality" \" ">" \" cardinality \" "^^xsd:nonNegativeInteger" "</span>")]
     ;fill (translate filler subject2label "owl:allValuesFrom")]
     (renderRestriction opening prop modifer card))))

(defn translateObjectMinQualifiedCardinality 
  "Translate a ObjectMinCardinality expression"
  [ofn subject2label]
  ofn)

  
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


(defn translate
  "Translate OFN-S expression to manchester synax"
  ([ofn subject2label]
  (let [operator (first ofn)];
    (case operator
      "ObjectSomeValuesFrom"  (translateObjectSomeValuesFrom ofn subject2label)
      "ObjectAllValuesFrom"  (translateObjectAllValuesFrom ofn subject2label)
      "ObjectMinCardinality"  (translateObjectMinCardinality ofn subject2label)
      ;"ObjectMaxCardinality"  (translateObjectMaxCardinality ofn)
      ;"ObjectExactCardinality"  (translateObjectMaxCardinality ofn)
      ;"ObjectHasValue"  (translateObjectHasValue ofn)
      ;"ObjectIntersectionOf"  (translateObjectIntersection ofn)
      ;"ObjectUnionOf"  (translateObjectUnion ofn)
      ;"ObjectOneOf"  (translateObjectOneOf ofn)
      ;"ObjectComplementOf"  (translateObjectComplement ofn)
      ;"ObjectHasSelf"  (translateObjectHasSelf ofn)
      (baseTranslation ofn subject2label))))
  ([ofn subject2label property]
   (let [operator (first ofn)]
     (case operator
      "ObjectSomeValuesFrom" (translateObjectSomeValuesFrom ofn subject2label property)
      "ObjectAllValuesFrom" (translateObjectAllValuesFrom ofn subject2label property)
      "ObjectMinCardinality"  (translateObjectMinCardinality ofn subject2label property)
      (baseTranslation ofn subject2label property)))))


