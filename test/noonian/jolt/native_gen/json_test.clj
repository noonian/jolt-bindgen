(ns noonian.jolt.native-gen.json-test
  (:require [clojure.test :refer [deftest is testing]]
            [noonian.jolt.native-gen.json :as json]))

(deftest scalars
  (is (= 1 (json/parse "1")))
  (is (= -2.5 (json/parse "-2.5")))
  (is (= 1.0e3 (json/parse "1e3")))
  (is (= "a\"b\\c\né" (json/parse "\"a\\\"b\\\\c\\n\\u00e9\"")))
  (is (true? (json/parse "true")))
  (is (false? (json/parse " false ")))
  (is (nil? (json/parse "null"))))

(deftest collections
  (is (= [] (json/parse "[]")))
  (is (= {} (json/parse "{ }")))
  (is (= [1 [2 [3]] {"a" nil}] (json/parse "[1,[2,[3]],{\"a\":null}]")))
  (is (= {"k" "v" "n" [1 2] "o" {"x" 1.5}} (json/parse "{\"k\":\"v\",\"n\":[1,2],\"o\":{\"x\":1.5}}"))))

(deftest on-pair-hook
  (testing "the hook sees pairs in document order with the parent key, and can add pairs"
    (let [seen (atom [])
          v (json/parse "{\"loc\":{\"file\":\"a.h\"},\"name\":\"x\",\"inner\":[{\"loc\":{\"line\":3},\"name\":\"y\"}]}"
                        {:on-pair (fn [parent k v]
                                    (swap! seen conj [parent k])
                                    (when (= k "loc") [["_seen" true]]))})]
      ;; a pair is reported once its value is fully read, so nested pairs
      ;; come before the pair that contains them
      (is (= [["loc" "file"] [nil "loc"] [nil "name"] ["loc" "line"] ["inner" "loc"] ["inner" "name"] [nil "inner"]]
             (->> @seen (remove #(= "_seen" (second %))) vec)))
      (is (true? (get v "_seen")))
      (is (true? (get-in v ["inner" 0 "_seen"]))))))

(deftest errors
  (is (thrown? Exception (json/parse "[1,")))
  (is (thrown? Exception (json/parse "{\"a\" 1}")))
  (is (thrown? Exception (json/parse "1 2"))))

(deftest documents-in-a-row
  (is (= [{"a" 1} [2] {"b" nil}] (json/parse-all "{\"a\": 1}\n[2]\n\n{\"b\": null}\n")))
  (is (= [] (json/parse-all "  \n"))))
