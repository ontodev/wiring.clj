(defproject wiring "0.1.0-SNAPSHOT" 
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "BSD 3-Clause License"
            :url "https://opensource.org/licenses/BSD-3-Clause"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [cheshire "5.10.0"]
                 [hiccup "1.0.5"]
                 [instaparse "1.4.10"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [org.xerial/sqlite-jdbc "3.36.0"]
                 [org.clojure/data.csv "1.0.0"]]
  :plugins [[lein-cljfmt "0.7.0"]] 
  :main ^:skip-aot wiring.ldtab2ofn.axiomTranslation.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
