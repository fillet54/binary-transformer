(defproject binary-transformer "0.1.0-SNAPSHOT"
  :description "Utilities for reading and writing binary data"
  :url "http://philgomez.com/binary-transformer"
  :license {:name "MIT License"
            :url "https://github.com/fillet54/binary-transformer/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha9"]]

  :main ^:skip-aot binary-transformer.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/test.check "0.9.0"]]}})
