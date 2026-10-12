(ns native-gen.macros-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [native-gen.macros :as macros]
            [native-gen.fixture :as fx]))

(def dd-output
  "What clang -E -dD prints: line markers naming the file, then directives."
  (str/join "\n"
            ["# 1 \"/sdk/other.h\""
             "#define FX_FROM_OTHER 1"
             "# 1 \"/proj/fixture.h\""
             "#define FX_FIXTURE_h_ 1"
             "#define FX_VERSION 3"
             "#define FX_NAME \"fixture\""
             "#define FX_FUNCTION_LIKE(x) ((x) + 1)"
             "#define FX_EMPTY"
             "#define FX_EXCLUDED 1"
             "#define FX_STATEMENT { 1; }"
             "#define FX_ATTR __attribute__((packed))"
             "#define FX_BUILTIN __builtin_expect"
             "#define OTHER_PREFIX 4"
             "# 1 \"/proj/fixture.h\" 2"
             "#define FX_LAST (1 << 2)"]))

(deftest candidates
  (is (= {"FX_VERSION" {:body "3" :file "/proj/fixture.h"}
          "FX_NAME" {:body "\"fixture\"" :file "/proj/fixture.h"}
          "FX_LAST" {:body "(1 << 2)" :file "/proj/fixture.h"}}
         (macros/candidates dd-output {:prefixes ["FX_"] :exclude #{"FX_EXCLUDED"}
                                       :file-pred #(str/includes? % "fixture.h")}))))

(deftest skipped
  (testing "a wanted macro that cannot be a constant is reported with the reason"
    (is (= {"FX_FIXTURE_h_" "include guard"
            "FX_FUNCTION_LIKE" "function-like"
            "FX_EMPTY" "no value"
            "FX_STATEMENT" "not a constant expression"
            "FX_ATTR" "not a constant expression"
            "FX_BUILTIN" "not a constant expression"}
           (:skipped (macros/scan dd-output {:prefixes ["FX_"] :exclude #{"FX_EXCLUDED"}
                                             :file-pred #(str/includes? % "fixture.h")})))))
  (testing "a later definition replaces an earlier one"
    (is (= {:candidates {"FX_A" {:body "2" :file "/proj/fixture.h"}} :skipped {}}
           (macros/scan "# 1 \"/proj/fixture.h\"\n#define FX_A\n#define FX_A 2"
                        {:prefixes ["FX_"] :exclude #{} :file-pred (constantly true)})))))

(deftest evaluate-fixture
  (let [m @fx/manifest
        cands (macros/candidates @fx/preprocessed {:prefixes (:macro-prefixes m)
                                                   :exclude (:macro-exclude m)
                                                   :file-pred #(str/includes? % "fixture.h")})
        {:keys [values pruned]} (macros/evaluate m cands)
        value (fn [n] (dissoc (values n) :file))]
    (is (= {:type :int :value "3"} (value "FX_VERSION")))
    (is (= {:type :string :value "fixture"} (value "FX_NAME")))
    (is (= {:type :double :value 1.5} (value "FX_SCALE")))
    (is (= {:type :uint :value "18446744073709551615"} (value "FX_BIG")))
    (is (= {:type :uint :value "8"} (value "FX_FLAG")))
    (is (= {:type :int :value "-7"} (value "FX_NEGATIVE")))
    (is (= {:type :bool :value true} (value "FX_TRUE")))
    (is (str/ends-with? (:file (values "FX_VERSION")) "fixture.h"))
    (testing "a macro that is not an expression is pruned with the compiler's words, and the rest survive"
      (is (= #{"FX_TYPE_NAME"} (set (keys pruned))))
      (is (string? (pruned "FX_TYPE_NAME"))))
    (is (= #{"FX_VERSION" "FX_NAME" "FX_SCALE" "FX_BIG" "FX_FLAG" "FX_NEGATIVE" "FX_TRUE"}
           (set (keys values))))))
