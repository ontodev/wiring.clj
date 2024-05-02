(ns wiring.ofn2rdfa.core
  (:require [clojure.repl :as repl]
            [wiring.ofn2rdfa.axiomTranslation :as axiomTranslation]
            [wiring.util.thickTriples :as thickTriples]
            [wiring.thick2ofn.parsing :as p]
            [wiring.typing.typeHandling :as typeHandling]
            [wiring.labelling.labelHandling :as labelHandling]
            [wiring.thick2ofn.axiomTranslation.translate :as thick2ofn]
            [hiccup.core :as hicc]
            [cheshire.core :as cs]
            [clojure.java.io :as io])
  (:gen-class))

(defn -main
  "Currently only used for manual testing."
  [& args]
  (def subject2types (hash-map))
  (def thickTriples #{})
  (def thinTriples #{});which are not typed
  (def subject2labels (hash-map));deal with other annotations later
  (def subclasses #{})
  (def classAxioms #{})

  ;(with-open [rdr (io/reader (io/resource "tests/thickClassExpressions.txt"))]
  ;(with-open [rdr (io/reader (io/resource "tests/thickClassExpressions.txt"))]
  (with-open [rdr (io/reader (io/resource "tests/thickOBO.txt"))]
    (doseq [line (line-seq rdr)]
      ;building the type map
      (if (thickTriples/typingTriple? (p/parse line))
        (def subject2types (thickTriples/updateSubject2info subject2types (p/parse line))))

      ;collect thick triples
      (if (and (thickTriples/thickTriple? (p/parse line))
               (not (thickTriples/typingTriple? (p/parse line))))
        (def thickTriples (conj thickTriples (p/parse line))))

      ;collect thin triples that are NOT types 
      (if (and (thickTriples/thinTriple? (p/parse line))
               (not (thickTriples/typingTriple? (p/parse line))))
        (def thinTriples (conj thinTriples (p/parse line))))

      (if (thickTriples/thinSubClassOf? (p/parse line))
        (def subclasses (conj subclasses (p/parse line))))

      (if (thickTriples/classAxiom? (p/parse line))
        (def classAxioms (conj classAxioms (p/parse line))))

      ;collect labeling triples
      (if (thickTriples/labelingTriple? (p/parse line))
        (def subject2labels (thickTriples/updateSubject2info subject2labels (p/parse line))))))

  ;translate all JSON thick triples to (abstract) OFN-S expressions
  ;(def ofnExpressions (map thick2ofn/translate thickTriples))
  ;;handle typing 
  ;(def typedExpressions (map #(typeHandling/translate (cs/parse-string %) subject2types) ofnExpressions))

  ;(def rdfaExpressions (map #(axiomTranslation/translate (cs/parse-string %) subject2labels) typedExpressions))

  (def ofnExpressions (map thick2ofn/translate classAxioms))

  ;handle typing 
  (def typedExpressions (map #(typeHandling/translate % subject2types) ofnExpressions))

  ;label handling
  (def labelSubstitution (map #(labelHandling/translate % subject2labels) typedExpressions))

  ;rdfa
  (def rdfaExpressions (map #(axiomTranslation/translate % subject2labels) typedExpressions))

  ;(def rdfaExpressions (map #(axiomTranslation/translate (cs/parse-string %) subject2labels) labelSubstitution))

  ;(println ofnExpressions)
  ;(println typedExpressions)
  ;(println labelSubstitution)
  ;(println subject2labels)
  ;(println classAxioms)
  ;(println typedExpressions)
  ;(println rdfaExpressions)

  (doseq [item rdfaExpressions]
    (println "===")
    (println item)
    (println (hicc/html item)))
  (println "==="))
