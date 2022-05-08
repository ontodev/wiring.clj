(ns wiring.ofn2thick.parsing
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [cheshire.core :as cs]
            [instaparse.core :as insta]
            [clojure.spec.alpha :as spec]))

;TODO handle single quotation marks {'subject': 'ex:A', 'predicate': 'rdf:type', 'object': 'owl:Class'} 
;TODO: handle quotations around thick map after 'object' 

(def firstParser
  (insta/parser
    "S = E
     E = BO (OP) BC | T | T W
     BO = '['
     BC = ']'
     OP = P W E*
     W =#'\\s'
     P = '\"SubClassOf\"' | '\"ObjectSomeValuesFrom\"'
     T = #'\"(\\S)*\"'"
    :output-format :enlive))

;TODO: first prototype
(def ofnParser
  (insta/parser
    "S = Subclass | EquivalentClasses | DisjointClasses | DisjointUnion
     <E> = T | OSF
     Subclass = <'[\"'> 'SubClassOf' <'\"'> <#'\\s'> LHS <#'\\s'> RHS <']'>
     LHS = E
     RHS = E
     RHSS = (#'\\s' E ']')+
     EquivalentClasses = '[\"EquivalentClasses\"' E (#'\\s' E ']')+
     DisjointClasses = '[\"DisjointClasses\"' E (#'\\s' E ']')+
     DisjointUnion = '[\"DisjointUnion\"' LHS RHSS
     OSF = <'[\"'> 'ObjectSomeValuesFrom' <'\"'> <#'\\s'> E <#'\\s'> E <']'>
     T = #'\"(\\S)*\"'" ))
    ;:output-format :enlive))


(defn testParse [x y]
  (println (str "Test Parse says: " x " " y)))

(defn parse
  "Parse OFN-S expression"
  [predicateMap]
  ;{:pre [(spec/valid? string? predicateMap)] ;a 'map' can be just a string, i.e. a normal object of a triple
   ;:post [(spec/valid? ::owlspec/map %)]}
  (let [testString1 "[\"SubClassOf\" \"ex:A\" \"ex:B\"]"
        testString2 "[\"SubClassOf\" \"ex:A\" [\"ObjectSomeValuesFrom\" \"ex:p\" \"ex:B\"]]" ]
    (println testString1)
    (println testString1)
    (println (firstParser testString1)) 
    (println "")
    (println (firstParser testString2)) 
    (println "")
    (println (insta/parses firstParser testString2)) 
    (println "")
    (println (ofnParser testString1)) 
    (println "")
    (println (ofnParser testString2)) 
    ))
