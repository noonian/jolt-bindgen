(ns noonian.jolt.native-gen.tests
  "Every test namespace: `jolt -A:test -m noonian.jolt.native-gen.tests`. Needs clang."
  (:require [clojure.test :as t]
            noonian.jolt.native-gen.json-test
            noonian.jolt.native-gen.ast-test
            noonian.jolt.native-gen.types-test
            noonian.jolt.native-gen.macros-test
            noonian.jolt.native-gen.emit-test
            noonian.jolt.native-gen.core-test))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-all-tests #"noonian\.jolt\.native-gen\..*-test")]
    (System/exit (if (zero? (+ fail error)) 0 1))))
