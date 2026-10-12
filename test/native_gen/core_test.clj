(ns native-gen.core-test
  "The classifier over the fixture, then a whole run: the files it writes,
  and the generated bindings loaded against the fixture library and called."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.edn :as edn]
            [jolt.ffi :as ffi]
            [native-gen.core :as core]
            [native-gen.fixture :as fx]))

(deftest classify
  (let [ir @fx/ir
        verdicts (into {} (map (fn [[k f why]] [(:name f) [k why]])
                               (map #(core/classify-function ir (:blocking @fx/manifest) %) (:functions ir))))]
    (is (= [:ok nil] (verdicts "fx_add")))
    (testing "a variadic function is bound with the bare marker"
      (is (= [:ok nil] (verdicts "fx_log"))))
    (testing "a typedef from another header resolves"
      (is (= [:ok nil] (verdicts "fx_mtime"))))
    (testing "a function returning a function pointer returns a pointer"
      (is (= [:ok nil] (verdicts "fx_handler"))))
    (testing "a static function has no symbol in the library"
      (is (= :skipped (first (verdicts "fx_twice")))))
    (is (= [:skipped "va_list"] (verdicts "fx_vlog")))
    (is (= [:skipped "variadic and :blocking do not combine"] (verdicts "fx_log_blocking")))
    (is (= [:skipped "variadic with a by-value aggregate"] (verdicts "fx_sum_points")))
    (is (= [:skipped "union FX_Value by value holds a union: pass it by pointer"] (verdicts "fx_value")))
    (is (= [:skipped "struct FX_Tagged by value holds a union: pass it by pointer"] (verdicts "fx_tagged")))
    (is (= [:skipped "struct FX_Bits by value: bitfield a in FX_Bits"] (verdicts "fx_bits")))))

(def generated
  "One generator run over the fixture, shared by the tests below."
  (delay (core/generate! @fx/manifest)))

(defn- source-path []
  (str (io/file (:out-dir @fx/manifest) "fixture" "ffi.jolt")))

(deftest generate-fixture
  (let [m @fx/manifest
        report @generated
        source (slurp (source-path))
        target (edn/read-string (slurp (core/target-file m (core/host-target))))
        layouts (:layouts target)]
    (testing "the report, written and returned alike"
      (is (= report (edn/read-string (slurp (:report-out m)))))
      (is (= '[fixture.ffi] (map :ns (:namespaces report))))
      (is (= {:functions 18 :records 8 :enums 3 :constants 9 :callbacks 2}
             (:counts (first (:namespaces report)))))
      (is (= #{"fx_vlog" "fx_log_blocking" "fx_sum_points" "fx_value" "fx_tagged" "fx_bits" "fx_twice"}
             (set (keys (:skipped-functions report)))))
      (is (= #{"FX_Bits" "FX_Anonymous"} (set (keys (:skipped-records report)))))
      (is (= ["FX_Opaque"] (:opaque-records report)))
      (is (= {"FX_VaCallback" "va_list"} (:skipped-callbacks report)))
      (is (= {"fx_counter" "global variable of type int"} (:skipped-variables report)))
      (is (= {"FX_FUNCTION_LIKE" "function-like"} (:skipped-macros report)))
      (is (= #{"FX_TYPE_NAME" "FX_NULL"} (set (keys (:pruned-macros report)))))
      (is (= {} (:skipped-names report))))
    (testing "the layouts were measured for every emitted record"
      (is (= #{"FX_Point" "FX_Rect" "FX_Value" "FX_Tagged" "FX_Named" "FX_Matrix" "FX_Vec2" "FX_Plain"} (set (keys layouts))))
      (is (= [8 4] (get-in layouts ["FX_Point" :size+align])))
      (is (= 4 (get-in layouts ["FX_Point" :fields "y"])))
      (is (= 8 (get-in layouts ["FX_Rect" :fields "size"]))))
    (testing "the generated layout test"
      (is (.exists (io/file (:test-out m) "fixture" "layout_test.clj"))))
    (testing "the source"
      (is (str/includes? source "(ffi/defcfn fx_add \"fx_add\" [:int :int] :int)"))
      (is (str/includes? source "(ffi/defcfn fx_log \"fx_log\" [:string :&] :void)"))
      (is (str/includes? source "(ffi/defcfn fx_wait \"fx_wait\" [:int] :void :blocking)"))
      (is (str/includes? source "(ffi/defcfn fx_say \"fx_say\" [:pointer :int] :void :blocking)"))
      (is (re-find #"\(ffi/defcfn fx_mtime \"fx_mtime\" \[:pointer\] :(long|int64)\)" source))
      (is (str/includes? source "(ffi/defcfn fx_handler \"fx_handler\" [:int] :pointer)"))
      (is (str/includes? source "(ffi/defcfn fx_dup \"fx_dup\" [:string] :pointer)"))
      (is (str/includes? source "(ffi/defcfn fx_const_name \"fx_const_name\" [:string] :string)"))
      (is (str/includes? source "(ffi/defcfn fx_origin \"fx_origin\" [] [:by-value [:struct [[:x :int32] [:y :int32]]]])"))
      (is (str/includes? source "(def FX_BIG 18446744073709551615N)"))
      (is (str/includes? source "(def FX_MODE_B 5)"))
      (is (str/includes? source "(def FX_ANON_Y 2)"))
      (is (str/includes? source "(def FX_LOOSE_B 4)"))
      (testing "an enum constant given as an unsigned expression, and the one after it"
        (is (str/includes? source "(def FX_LOOSE_U 16)"))
        (is (str/includes? source "(def FX_LOOSE_N 17)")))
      (is (str/includes? source "(def FX_Callback {"))
      (is (str/includes? source "(def FX_Nested {:args [:int :pointer :pointer], :ret :void})"))
      (is (not (str/includes? source "FX_VaCallback")))
      (is (not (str/includes? source "fx_twice")))
      (is (not (str/includes? source "fx_vlog"))))))

(defn- build-library!
  "Compile fixture.c into a shared library in the work directory; its path."
  []
  (let [out (str (io/file (:work-dir @fx/manifest)
                          (case (core/os-key) :darwin "libfixture.dylib" :windows "fixture.dll" "libfixture.so")))
        {:keys [exit err]} (sh/sh "clang" "-shared" "-fPIC" "-w" "-o" out
                                  (str "-I" fx/dir) (str (io/file fx/dir "fixture.c")))]
    (when-not (zero? exit)
      (throw (ex-info "fixture library failed to build" {:err err})))
    out))

(deftest generated-bindings-run
  @generated
  (ffi/load-library (build-library!))
  (load-file (source-path))
  (let [value (fn [sym] @(ns-resolve 'fixture.ffi sym))
        call (fn [sym & args] (apply (value sym) args))
        layout value]
    (is (= 7 (call 'fx_add 3 4)))
    (is (= "fixture" (call 'fx_name)))
    (is (= 5 (call 'fx_len "hello")))
    (is (= 3 (value 'FX_VERSION)))
    (is (= 18446744073709551615N (value 'FX_BIG)))
    (testing "a struct returned by value arrives through a destination pointer"
      (ffi/with-layout [p (layout 'FX_Point)]
        (call 'fx_origin p)
        (is (= {:x 3 :y 4} (ffi/read p (layout 'FX_Point))))))
    (testing "a struct named only by its typedef"
      (ffi/with-layout [v (layout 'FX_Vec2)]
        (call 'fx_vec2 v 1.5 2.5)
        (is (= {:x 1.5 :y 2.5} (ffi/read v (layout 'FX_Vec2))))))
    (testing "a struct named only by its tag"
      (ffi/with-layout [p (layout 'FX_Plain)]
        (ffi/write-field p (layout 'FX_Plain) [:a] 2)
        (ffi/write-field p (layout 'FX_Plain) [:b] 0.5)
        (is (= 2.5 (call 'fx_plain p)))))
    (testing "a struct passed by value goes as a pointer to its bytes"
      (ffi/with-layout [r (layout 'FX_Rect)]
        (ffi/write-field r (layout 'FX_Rect) [:origin :x] 1)
        (ffi/write-field r (layout 'FX_Rect) [:origin :y] 0)
        (ffi/write-field r (layout 'FX_Rect) [:size 0] 2.0)
        (ffi/write-field r (layout 'FX_Rect) [:size 1] 3.0)
        (ffi/write-field r (layout 'FX_Rect) [:tag] 4)
        (is (= 11 (call 'fx_area r)))))
    (testing "a variadic call reads its tail per call"
      (call 'fx_log "%s=%d" "n" 42)
      (is (= "n=42" (call 'fx_last_log)))
      (call 'fx_log "plain")
      (is (= "plain" (call 'fx_last_log))))
    (testing "a blocking call takes its C string as a pointer"
      (ffi/with-c-string [s "hi"]
        (call 'fx_say s 2))
      (is (= "hi/2" (call 'fx_last_log))))
    (testing "a typedef from another header crosses as its C type"
      (let [o (call 'fx_open "x")]
        (is (= 7000 (call 'fx_mtime o)))
        (call 'fx_close o)))
    (testing "every emitted layout matches the compiler's measurement"
      (doseq [[name {:keys [size+align]}] (:layouts (edn/read-string (slurp (core/target-file @fx/manifest (core/host-target)))))
              :let [l (layout (symbol name))]]
        (is (= size+align [(ffi/sizeof l) (ffi/alignof l)]) name)))))

;; --- several targets -------------------------------------------------------

(def windows "windows-x86_64")

(def unix
  "A target that is not Windows: this machine, unless it is."
  (if (= windows (core/host-target)) "linux-x86_64" (core/host-target)))

(def portable
  "A manifest for the header that any target can be extracted from, asking
  for a Windows and a non-Windows target; one of them is this machine."
  (delay
    (let [work (io/file (System/getProperty "java.io.tmpdir")
                        (str "native-gen-portable-" (System/currentTimeMillis)))]
      {:headers ["portable.h"]
       :include-dirs [fx/dir]
       :file-filter "portable.h"
       :skip-headers #{}
       :ns-prefix "portable"
       :ns-names {"portable.h" "portable.ffi"}
       :work-dir (str (io/file work "work"))
       :targets-dir (str (io/file work "targets"))
       :out-dir (str (io/file work "src"))
       :test-out (str (io/file work "test"))
       :test-ns "portable.layout-test"
       :report-out (str (io/file work "report.edn"))
       :macro-prefixes ["PT_"]
       :macro-exclude #{"PT_CALL"}
       :blocking #{}
       :targets [windows unix]})))

(def portable-report (delay (core/generate! @portable)))

(defn- recorded [target]
  (edn/read-string (slurp (core/target-file @portable target))))

(deftest targets-on-record
  @portable-report
  (testing "each target is recorded as clang saw it, the one that is not this machine marked"
    (is (= #{windows unix} (set (keys (core/read-targets @portable)))))
    (is (false? (:cross? (recorded (core/host-target)))))
    (is (true? (:cross? (recorded (if (= windows (core/host-target)) unix windows))))))
  (testing "files are named as the header is included, not as this machine finds it"
    (is (= #{"portable.h"} (set (map :file (:functions (recorded unix)))))))
  (testing "a typedef from another header is kept only when a declaration uses it"
    (is (= ["size_t"] (keys (:system-typedefs (recorded unix))))))
  (testing "what differs between them"
    (is (= "long" (get-in (recorded unix) [:typedefs "pt_off" :type])))
    (is (= "long long" (get-in (recorded windows) [:typedefs "pt_off" :type])))
    (is (= "/" (get-in (recorded unix) [:macros "PT_SEPARATOR" :value])))
    (is (= "\\" (get-in (recorded windows) [:macros "PT_SEPARATOR" :value])))
    (is (= [24 8] (get-in (recorded unix) [:layouts "PT_Entry" :size+align])))
    (is (= [16 8] (get-in (recorded windows) [:layouts "PT_Entry" :size+align])))
    (is (= 4 (get-in (recorded windows) [:layouts "PT_Entry" :fields "size"])))))

(deftest merged-bindings
  (let [report @portable-report
        source (slurp (str (io/file (:out-dir @portable) "portable" "ffi.jolt")))
        test-source (slurp (str (io/file (:test-out @portable) "portable" "layout_test.clj")))]
    (testing "the report names the targets and what they do not share"
      (is (= #{windows unix} (set (keys (:targets report)))))
      (is (= {"pt_windows_only" {:only [windows]}
              "pt_tell" {unix "(ffi/defcfn pt_tell \"pt_tell\" [:pointer] :long)"
                         windows "(ffi/defcfn pt_tell \"pt_tell\" [:pointer] :int64)"}
              "PT_SEPARATOR" {unix "(def PT_SEPARATOR \"/\")"
                              windows "(def PT_SEPARATOR \"\\\\\")"}}
             (get-in report [:divergent 'portable.ffi]))))
    (testing "a type that differs is chosen at load"
      (is (str/includes? source (str "(case native-gen-target\n"
                                     "  " (pr-str unix) " (ffi/defcfn pt_tell \"pt_tell\" [:pointer] :long)\n"
                                     "  " (pr-str windows) " (ffi/defcfn pt_tell \"pt_tell\" [:pointer] :int64))"))))
    (testing "what the targets share is written once"
      (is (str/includes? source "\n(def PT_VERSION 2)\n"))
      (is (str/includes? source "\n(def PT_Entry-desc [:struct [[:tag :char] [:size :long] [:next :pointer]]])\n"))
      (is (str/includes? source "\n(ffi/defcfn pt_windows_only \"pt_windows_only\" [] :int)\n")))
    (testing "a callback with a calling convention on one target is the same callback"
      (is (str/includes? source "\n(def PT_Visit {:args [:pointer :pointer], :ret :void})\n")))
    (testing "the layout test carries every target's measurements"
      (is (str/includes? test-source "(def measured"))
      (is (str/includes? test-source (pr-str windows)))
      (is (str/includes? test-source "(check \"PT_Entry\" #'r0/PT_Entry)")))
    (testing "loaded here, the bindings are this machine's"
      (load-file (str (io/file (:out-dir @portable) "portable" "ffi.jolt")))
      (let [value (fn [sym] @(ns-resolve 'portable.ffi sym))
            host (core/host-target)]
        (is (= host (value 'native-gen-target)))
        (is (= (if (= windows host) "\\" "/") (value 'PT_SEPARATOR)))
        (is (= (get-in (recorded host) [:layouts "PT_Entry" :size+align])
               [(ffi/sizeof (value 'PT_Entry)) (ffi/alignof (value 'PT_Entry))]))))))

(deftest checking-the-record
  @portable-report
  (let [host (core/host-target)
        other (if (= windows host) unix windows)
        file (core/target-file @portable host)
        text (slurp file)]
    (testing "a fresh extraction agrees with what was just recorded"
      (is (nil? (core/check @portable))))
    (testing "and says what differs when the record is stale"
      (spit file (str/replace text "\"pt_tell\"" "\"pt_told\""))
      (is (= {:target host :differs {:functions ["pt_tell" "pt_told"]}} (core/check @portable)))
      (spit file text))
    (testing "a target recorded on its own platform is not replaced by a view from this one"
      (let [other-file (core/target-file @portable other)
            native (str/replace (slurp other-file) ":cross? true" ":cross? false")]
        (spit other-file (str/replace native "\"pt_count\"" "\"pt_kept\""))
        (core/extract! @portable)
        (is (str/includes? (slurp other-file) "\"pt_kept\""))
        (spit other-file (str/replace native ":cross? false" ":cross? true"))
        (core/extract! @portable)
        (is (not (str/includes? (slurp other-file) "\"pt_kept\"")))))
    (testing "a target clang cannot parse from here is reported, and the rest go on"
      (is (= ["android-arm64"]
             (keys (core/extract! (assoc @portable
                                         :targets ["android-arm64"]
                                         :include-dirs {:darwin [fx/dir] :linux [fx/dir] :windows [fx/dir]}))))))))

