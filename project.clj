(defproject scherz "1.0.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [overtone/overtone "0.10.3"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [compojure "1.6.1"]
                 [ring/ring-jetty-adapter "1.7.1"]
                 [ring/ring-json "0.5.0"]
                 [environ "1.1.0"]]
  :native-path "native"
  :source-paths ["src"]
  :main scherz.generate
  :plugins [[environ/environ.lein "0.3.1"]]
  :hooks [environ.leiningen.hooks]
  :uberjar-name "scherz.jar"
  :profiles {:production {:env {:production true}}})


