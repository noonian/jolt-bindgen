(ns noonian.jolt.bindgen.main
  (:require [noonian.jolt.bindgen.core :as core]))

(defn -main [& args]
  (let [check? (some #{"--check"} args)
        manifest-path (first (remove #{"--check"} args))]
    (when-not manifest-path
      (println "usage: jolt -m noonian.jolt.bindgen.main [--check] path/to/manifest.edn")
      (System/exit 2))
    (let [manifest (core/load-manifest manifest-path)
          differs (if check?
                    (core/check manifest)
                    (do (core/generate! manifest) nil))]
      (when differs
        (println (:target differs) "is not what is on record for it:")
        (clojure.pprint/pprint (:differs differs)))
      ;; the threads clojure.java.shell ran clang on would hold the process open
      (shutdown-agents)
      (when differs
        (System/exit 1)))))
