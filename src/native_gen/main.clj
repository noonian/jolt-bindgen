(ns native-gen.main
  (:require [native-gen.core :as core]))

(defn -main [& [manifest-path]]
  (when-not manifest-path
    (println "usage: jolt -m native-gen.main path/to/manifest.edn")
    (System/exit 2))
  (core/generate! (core/load-manifest manifest-path))
  nil)
