(ns noonian.jolt.bindgen.tests
  "Every test namespace: `jolt -A:test -m noonian.jolt.bindgen.tests`. Needs clang."
  (:require [clojure.test :as t]
            noonian.jolt.bindgen.json-test
            noonian.jolt.bindgen.ast-test
            noonian.jolt.bindgen.types-test
            noonian.jolt.bindgen.macros-test
            noonian.jolt.bindgen.emit-test
            noonian.jolt.bindgen.core-test))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-all-tests #"noonian\.jolt\.bindgen\..*-test")]
    (System/exit (if (zero? (+ fail error)) 0 1))))
