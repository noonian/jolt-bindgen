(ns native-gen.fixture
  "The header under test/fixtures, run through clang once per test run: a
  manifest writing into a fresh temporary directory, the preprocessed text
  and the extracted IR."
  (:require [native-gen.clang :as clang]
            [native-gen.ast :as ast]
            [native-gen.core :as core]
            [clojure.java.io :as io]))

(def dir (str (io/file (System/getProperty "user.dir") "test" "fixtures")))

(def manifest
  (delay
    (let [work (io/file (System/getProperty "java.io.tmpdir")
                        (str "native-gen-test-" (System/currentTimeMillis)))]
      (.mkdirs work)
      {:headers ["fixture.h"]
       :include-dirs [dir]
       :file-filter "fixture.h"
       :skip-headers #{}
       :source-label "fixture"
       :ns-prefix "fixture"
       :ns-names {"fixture.h" "fixture.ffi"}
       :work-dir (str work)
       :out-dir (str (io/file work "src"))
       :test-out (str (io/file work "test"))
       :test-ns "fixture.layout-test"
       :layouts-out (str (io/file work "layouts.edn"))
       :report-out (str (io/file work "report.edn"))
       :macro-prefixes ["FX_"]
       :macro-exclude #{"FX_EXCLUDED"}
       :blocking #{"fx_wait" "fx_say" "fx_log_blocking"}})))

(def preprocessed (delay (clang/preprocess @manifest)))

(def ir
  (delay (ast/extract (ast/parse-dump (clang/ast-json @manifest @preprocessed))
                      (core/file-pred @manifest))))
