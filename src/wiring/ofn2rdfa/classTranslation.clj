(ns wiring.ofn2hiccup.classTranslation
  (:require [clojure.repl :as repl]
            [clojure.java.io :as io]
            [wiring.ofn2rdfa.propertyTranslation :as property])
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
  (let [label (labelSubstitution namedClass subject2label)]
    [:a {:href namedClass} label]))
   
  ([namedClass subject2label property]
  (let [label (labelSubstitution namedClass subject2label)]
    [:a {:href namedClass, :property property} label])))

(defn spanOpening
  "Determines the opening span for an RDFa serialistion."  
  ([input]
  (if (namedClass? input) 
    {:about input}
    {:typeof (getType input)})) 
  ([input property];RDFa property argument from parent call
   (if (namedClass? input)
      {:about input, :property property}
      {:typeof (getType input), :property property}))) 

(defn renderRestriction 
  "Renders a class restriction" 
  ;restrictions
  ([spanOpening property modifer filler]
   [:div "(" [:span spanOpening property modifer filler] ")"]) 
  ;qualified cardinality restrictions
  ([spanOpening property modifer card filler]
   [:div "(" [:span spanOpening property modifer card " " filler] ")"])) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      RDF lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TODO: there are ways to write lists in RDFa much more succintly.
;unfortunately, I couldn't get more succinct notations working in nested epxressions.

(defn translateList
  "Translate class expressions into an RDF list"
  [expressions subject2label htmlMarkup]
  (loop [in (rest (reverse expressions));constructing list from last element to first
         out [:span {:property "rdf:rest", :typeof "rdf:List"} 
              " " htmlMarkup " "
              (translate (first (reverse expressions)) subject2label "rdf:first")
              [:span {:resource "rdf:nil", :property "rdf:rest"}]]]
    (if (empty? in)
      out
      (recur (rest in)
             (if (empty? (rest in))
               [:span (translate (first in) subject2label "rdf:first") out]
               [:span {:property "rdf:rest", :typeof "rdf:List"} 
                " " htmlMarkup " "
                (translate (first in) subject2label "rdf:first")
                out])))))

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
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:minCardinality"} number] ] 
     (renderRestriction opening prop modifer card)))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " min ")
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:minCardinality"} number] ]
     (renderRestriction opening prop modifer card))))

(defn translateObjectMinQualifiedCardinality 
  "Translate a ObjectMinCardinality expression" 
  ([ofn subject2label]
  (let [[op cardinality property filler] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " min ")
     fill (translate filler subject2label "owl:onClass")
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:minQualifiedCardinality"} number]] 
    (renderRestriction opening prop modifer card fill))) 

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property filler] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " min ")
     fill (translate filler subject2label "owl:onClass")
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:minQualifiedCardinality"} number]] 
     (renderRestriction opening prop modifer card fill)))) 

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
     modifer (str " max ") 
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:maxCardinality"} number] ] 
     (renderRestriction opening prop modifer card)))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " max ")
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:minCardinality"} number] ]
     (renderRestriction opening prop modifer card))))

(defn translateObjectMaxQualifiedCardinality 
  "Translate a ObjectMaxCardinality expression" 
  ([ofn subject2label]
  (let [[op cardinality property filler] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " max ")
     fill (translate filler subject2label "owl:onClass")
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:minQualifiedCardinality"} number]] 
     (renderRestriction opening prop modifer card fill)))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property filler] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " max ")
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:minQualifiedCardinality"} number]
     fill (translate filler subject2label "owl:onClass")]
     (renderRestriction opening prop modifer card fill)))) 

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
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:minCardinality"} number] ] 
     (renderRestriction opening prop modifer card)))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " exactly ")
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:minCardinality"} number] ]
     (renderRestriction opening prop modifer card))))

(defn translateObjectExactQualifiedCardinality 
  "Translate a ObjectExactCardinality expression" 
  ([ofn subject2label]
  (let [[op cardinality property filler] ofn ;no parent RDFa property
     opening (spanOpening ofn)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " exactly ")
     fill (translate filler subject2label "owl:onClass")
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:minQualifiedCardinality"} number]] 
     (renderRestriction opening prop modifer card fill)))

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op cardinality property filler] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer (str " exactly ")
     fill (translate filler subject2label "owl:onClass")
     number (str cardinality "^^xsd:nonNegativeInteger")
     card [:span {:property "owl:minQualifiedCardinality"} number]] 
     (renderRestriction opening prop modifer card fill)))) 

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
     fill [:span {:property "owl:hasSelf", :hidden "true"} "true^^xsd:boolean" ]]
     (renderRestriction opening prop modifer fill))) 

  ([ofn subject2label propertyRDFa] ;parent RDFa property
   (let [[op property] ofn
     opening (spanOpening ofn propertyRDFa)
     prop (property/translate property subject2label "owl:onProperty")
     modifer " some Self "
     fill [:span {:property "owl:hasSelf", :hidden "true"} "true^^xsd:boolean" ]]
     (renderRestriction opening prop modifer fill))))

(defn translateObjectIntersection
  "Translate an ObjectIntersectionOf expression"
  ([ofn subject2label]
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn) 
        operands (translateList arguments subject2label "and")] 
    [:span htmlOpening [:span {:property "owl:intersectionOf", :typeof "rdf:List"} operands]]))
  ([ofn subject2label propertyRDFa]
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn propertyRDFa)
        operands (translateList arguments subject2label "and")]
    [:span htmlOpening [:span {:property "owl:intersectionOf", :typeof "rdf:List"} "(" operands ")"]])))

(defn translateObjectUnion
  "Translate an ObjectUnionOf expression"
  ([ofn subject2label]
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn) 
        operands (translateList arguments subject2label "or")]
    [:span htmlOpening [:span {:property "owl:unionOf", :typeof "rdf:List"} "(" operands ")"]]))

  ([ofn subject2label propertyRDFa]
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn propertyRDFa)
        operands (translateList arguments subject2label "or")]
    [:span htmlOpening [:span {:property "owl:unionOf", :typeof "rdf:List"} "(" operands ")"]])))


(defn translateObjectOneOf
  "Translate an ObjectOneOf expression"
([ofn subject2label] 
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn) 
        operands (translateList arguments subject2label "")]
    [:span htmlOpening [:span {:property "owl:oneOf", :typeof "rdf:List"} "{" operands "}"]]))

  ([ofn subject2label propertyRDFa]
  (let [[operator & arguments] ofn 
        htmlOpening (spanOpening ofn propertyRDFa)
        operands (translateList arguments subject2label "")]
    [:span htmlOpening [:span {:property "owl:oneOf", :typeof "rdf:List"} "{" operands "}"]])))


(defn translateObjectComplement
  "Translate an ObjectComplementOf expression"
  ([ofn subject2label]
  (let [[operator argument] ofn
    htmlOpen (spanOpening ofn)
    prop (translate argument subject2label "owl:complementOf")]
    [:span htmlOpen " not (" prop ")"]))
  ([ofn subject2label propertyRDFa]
  (let [[operator argument] ofn
    htmlOpen (spanOpening ofn propertyRDFa)
    prop (translate argument subject2label "owl:complementOf")]
    [:span htmlOpen " not (" prop ")"]))) 

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


