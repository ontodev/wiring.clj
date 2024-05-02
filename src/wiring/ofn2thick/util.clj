(ns wiring.ofn2thick.util
  (:require [clojure.repl :as repl]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]))

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
