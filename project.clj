(defproject binary-transformer "0.1.0-SNAPSHOT"
  :description "Utilities for reading and writing binary data"
  :url "http://philgomez.com/binary-transformer"
  :license {:name "MIT License"
            :url "https://github.com/fillet54/binary-transformer/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha12"]

                 ; tools.analyzer has a conflict with core.async. Excluding from core.match and
                 ; adding updated one manually. core.match hasn't been updated since Feb 2015
                 [org.clojure/core.match "0.3.0-alpha4" :exclusions [org.clojure/tools.analyzer.jvm]]
                 [org.clojure/tools.analyzer.jvm "0.6.10"]]

  :main ^:skip-aot binary-transformer.core
  :target-path "target/%s"
  :java-source-paths ["src/java"]
  :source-paths ["src/clj"]
  :test-paths ["test/clj"]
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/test.check "0.9.0"]]}})
