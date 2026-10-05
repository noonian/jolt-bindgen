(ns native-gen.tests
  "Every test namespace: `jolt -A:test -m native-gen.tests`. Needs clang."
  (:require [clojure.test :as t]
            native-gen.json-test
            native-gen.ast-test
            native-gen.types-test
            native-gen.macros-test
            native-gen.emit-test
            native-gen.core-test))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-all-tests #"native-gen\..*-test")]
    (System/exit (if (zero? (+ fail error)) 0 1))))
