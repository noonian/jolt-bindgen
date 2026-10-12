(ns native-gen.core
  "The generator driver. `generate!` takes a manifest map and writes the
  binding namespaces, the layout measurements, the layout test and a report."
  (:require [native-gen.clang :as clang]
            [native-gen.ast :as ast]
            [native-gen.types :as types]
            [native-gen.macros :as macros]
            [native-gen.emit :as emit]
            [native-gen.verify :as verify]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn os-key []
  (let [n (str/lower-case (or (System/getProperty "os.name") ""))]
    (cond (str/includes? n "mac") :darwin
          (str/includes? n "win") :windows
          :else :linux)))

(defn- per-os [v]
  (if (map? v) (get v (os-key)) v))

(defn load-manifest [path]
  (let [m (edn/read-string (slurp path))
        dir (or (.getParent (io/file path)) ".")
        project-dir (str (io/file dir (or (:project-dir m) "..")))]
    (-> m
        (assoc :manifest-dir dir :project-dir project-dir)
        (update :include-dirs per-os)
        (update :work-dir #(str (io/file dir (or % "work"))))
        (update :out-dir #(str (io/file project-dir (or % "src"))))
        (update :layouts-out #(str (io/file dir (or % (str "layouts." (name (os-key)) ".edn")))))
        (update :test-out #(str (io/file project-dir (or % "test"))))
        (update :report-out #(str (io/file dir (or % "report.edn"))))
        (update :blocking set)
        (update :macro-exclude set)
        (update :skip-headers set))))

(defn file-pred
  "Whether a declaration's file is one the manifest wants bindings for."
  [{:keys [file-filter skip-headers]}]
  (fn [file]
    (and (str/includes? file file-filter)
         (not (contains? skip-headers (last (str/split file #"/")))))))

(defn classify-function
  "[:ok f] when the function can be bound, else [:skipped f reason]. A
  static function is defined by the header and has no symbol in the library.
  A variadic function is bound with the bare `:&` marker, which jolt cannot
  combine with :blocking or with an aggregate by value anywhere in the
  signature; a va_list parameter has no jolt spelling at all."
  [ctx blocking f]
  (let [ps (map #(types/param-type ctx (:type %)) (:params f))
        r (types/return-type ctx (:ret f))
        bad (concat (filter map? ps) (when (map? r) [r]))
        aggregate? (or (some vector? ps) (vector? r))]
    (cond
      (:static? f) [:skipped f "static function: the library exports no symbol for it"]
      (seq bad) [:skipped f (str/join "; " (map :unsupported bad))]
      (and (:variadic? f) (contains? blocking (:name f))) [:skipped f "variadic and :blocking do not combine"]
      (and (:variadic? f) aggregate?) [:skipped f "variadic with a by-value aggregate"]
      :else [:ok f])))

(defn- write! [path text]
  (io/make-parents path)
  (spit path text)
  path)

(defn generate! [{:keys [headers out-dir layouts-out test-out test-ns report-out
                         macro-prefixes macro-exclude blocking source-label] :as manifest}]
  (let [pred (file-pred manifest)
        _ (println "preprocessing" headers)
        pre (clang/preprocess manifest)
        _ (println "dumping and parsing the AST")
        ir (ast/extract (ast/parse-dump (clang/ast-json manifest pre)) pred)
        ctx ir
        _ (println "evaluating macros")
        scanned (macros/scan pre {:prefixes macro-prefixes :exclude macro-exclude :file-pred pred})
        {:keys [values pruned]} (macros/evaluate manifest (:candidates scanned))
        classified (map #(classify-function ctx blocking %) (:functions ir))
        functions (map second (filter #(= :ok (first %)) classified))
        skipped-fns (into {} (map (fn [[_ f why]] [(:name f) why]) (remove #(= :ok (first %)) classified)))
        callback-verdicts (into {} (keep (fn [[n td]] (when-let [sig (types/callback-signature ctx (:type td))]
                                                        [n (assoc sig :file (:file td))]))
                                       (:typedefs ir)))
        callbacks (into {} (remove (comp :unsupported val) callback-verdicts))
        skipped-callbacks (into {} (keep (fn [[n sig]] (when-let [why (:unsupported sig)] [n why])) callback-verdicts))
        ;; a global is data in the library, not a function to bind
        skipped-variables (into {} (map (fn [v] [(:name v) (str "global variable of type " (:type v))]) (:variables ir)))
        enums (concat (sort-by :name (vals (:enums ir))) (:anonymous-enums ir))
        complete (filter :complete? (vals (:records ir)))
        ;; a record with a bitfield or an anonymous member has no jolt
        ;; layout; it is reported and left out, and pointers to it still work
        records (filter #(types/try-descriptor ctx %) complete)
        skipped-records (into {} (keep (fn [r] (when-let [why (types/unrepresentable-reason ctx r)] [(:name r) why]))) complete)
        by-file (fn [xs] (group-by :file xs))
        files (set (concat (keys (by-file functions)) (keys (by-file records))
                           (map :file enums) (map :file (vals values)) (map :file (vals callbacks))))
        _ (println "emitting" (count files) "namespaces")
        emitted (for [file (sort files)
                      :let [ns-sym (symbol (emit/header-ns manifest file))
                            in-file #(= file (:file %))
                            parts {:functions (sort-by :name (filter in-file functions))
                                   :records (sort-by :name (filter in-file records))
                                   :enums (filter in-file enums)
                                   :constants (into {} (filter (comp in-file val) values))
                                   :callbacks (into {} (map (fn [[n s]] [n (dissoc s :file)])
                                                            (filter (comp in-file val) callbacks)))}
                            {:keys [source skipped]} (emit/render-namespace
                                                      {:ctx ctx :blocking blocking :header-file (last (str/split file #"/"))
                                                       :source-label source-label}
                                                      ns-sym parts)
                            path (write! (str (io/file out-dir (emit/ns-file ns-sym))) source)]]
                  {:ns ns-sym :file path :records (:records parts) :skipped skipped
                   :counts (into {} (map (fn [[k v]] [k (count v)]) parts))})
        _ (println "measuring layouts")
        measured (verify/measure manifest ctx records)
        _ (write! layouts-out (with-out-str (clojure.pprint/pprint measured)))
        ns->records (into {} (keep (fn [{:keys [ns records]}] (when (seq records) [ns records])) emitted))
        test-path (str (io/file test-out (emit/ns-file (symbol test-ns))))
        test-path (str/replace test-path #"\.jolt$" ".clj")
        _ (write! test-path (verify/test-source test-ns measured ns->records))
        report {:os (os-key)
                :namespaces (mapv #(select-keys % [:ns :counts]) emitted)
                :skipped-functions skipped-fns
                :skipped-records skipped-records
                :skipped-callbacks skipped-callbacks
                :skipped-variables skipped-variables
                :skipped-macros (:skipped scanned)
                :pruned-macros pruned
                :skipped-names (into {} (mapcat :skipped emitted))
                :opaque-records (sort (map :name (remove :complete? (vals (:records ir)))))}]
    (write! report-out (with-out-str (clojure.pprint/pprint report)))
    (println "functions" (count functions) "skipped" (count skipped-fns)
             "| records" (count records) "skipped" (count skipped-records) "| enums" (count enums)
             "| constants" (count values) "pruned" (count pruned) "skipped" (count (:skipped scanned))
             "| callbacks" (count callbacks) "skipped" (count skipped-callbacks)
             "| variables skipped" (count skipped-variables))
    (println "report:" report-out)
    report))
