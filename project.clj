(defproject codestats "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [expectations "1.4.52"]
                 [iota "1.1.1"]
                 [criterium "0.4.2"]]
  :profile {:dev {:dependencies [[expectations "1.4.52"]]}}
  :plugins [[lein-autoexpect "1.0"]]
  :jvm-opts ["-Xmx1g"]
  :main contest.core)
