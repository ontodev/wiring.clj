(defproject wiring "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "BSD 3-Clause License"
            :url "https://opensource.org/licenses/BSD-3-Clause"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [cheshire "5.10.0"]
                 [hiccup "1.0.5"]
                 [instaparse "1.4.10"]]
  :plugins [[lein-cljfmt "0.7.0"]] 
  ;:main ^:skip-aot wiring.thick2ofn.core
  ;:main ^:skip-aot wiring.ofn2thick.core
  ;:main ^:skip-aot wiring.ofn2man.core
  ;:main ^:skip-aot wiring.cli.core
  ;:main ^:skip-aot wiring.thick2owl.core
  :main ^:skip-aot wiring.ofn2rdfa.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
