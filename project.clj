(defproject scherz "1.0.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [compojure "1.6.1"]
                 [ring/ring-jetty-adapter "1.7.1"]
                 [ring/ring-json "0.5.0"]
                 [ring-cors "0.1.13"]
                 [environ "1.1.0"]]
  :native-path "native"
  :source-paths ["src"]
  :main scherz.server
  :plugins [[environ/environ.lein "0.3.1"]]
  :hooks [environ.leiningen.hooks]
  :uberjar-name "scherz.jar"
  :uberjar-exclusions [#"scherz/play.clj"]
  :profiles {:dev {:dependencies [[overtone/overtone "0.10.3"]]}}
  :min-lein-version "2.0.0")

