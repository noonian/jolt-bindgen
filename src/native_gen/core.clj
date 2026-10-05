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
  variadic function is bound with the bare `:&` marker, which jolt cannot
  combine with :blocking or with an aggregate by value anywhere in the
  signature; a va_list parameter has no jolt spelling at all."
  [ctx blocking f]
  (let [ps (map #(types/param-type ctx (:type %)) (:params f))
        r (types/return-type ctx (:ret f))
        bad (concat (filter map? ps) (when (map? r) [r]))
        aggregate? (or (some vector? ps) (vector? r))]
    (cond
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
        {:keys [values pruned]} (macros/evaluate manifest
                                                 (macros/candidates pre {:prefixes macro-prefixes
                                                                         :exclude macro-exclude
                                                                         :file-pred pred}))
        classified (map #(classify-function ctx blocking %) (:functions ir))
        functions (map second (filter #(= :ok (first %)) classified))
        skipped-fns (into {} (map (fn [[_ f why]] [(:name f) why]) (remove #(= :ok (first %)) classified)))
        callbacks (into {} (keep (fn [[n td]] (when-let [sig (types/callback-signature ctx (:type td))]
                                                [n (assoc sig :file (:file td))]))
                               (:typedefs ir)))
        complete (filter :complete? (vals (:records ir)))
        ;; a record with a bitfield or an anonymous member has no jolt
        ;; layout; it is reported and left out, and pointers to it still work
        records (filter #(types/try-descriptor ctx %) complete)
        skipped-records (into {} (keep (fn [r] (when-let [why (types/unrepresentable-reason ctx r)] [(:name r) why]))) complete)
        by-file (fn [xs] (group-by :file xs))
        files (set (concat (keys (by-file functions)) (keys (by-file records))
                           (map :file (vals (:enums ir))) (map :file (vals values)) (map :file (vals callbacks))))
        _ (println "emitting" (count files) "namespaces")
        emitted (for [file (sort files)
                      :let [ns-sym (symbol (emit/header-ns manifest file))
                            in-file #(= file (:file %))
                            parts {:functions (sort-by :name (filter in-file functions))
                                   :records (sort-by :name (filter in-file records))
                                   :enums (sort-by :name (filter in-file (vals (:enums ir))))
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
                :pruned-macros pruned
                :skipped-names (into {} (mapcat :skipped emitted))
                :opaque-records (sort (map :name (remove :complete? (vals (:records ir)))))}]
    (write! report-out (with-out-str (clojure.pprint/pprint report)))
    (println "functions" (count functions) "skipped" (count skipped-fns)
             "| records" (count records) "skipped" (count skipped-records) "| enums" (count (:enums ir))
             "| constants" (count values) "pruned" (count pruned)
             "| callbacks" (count callbacks))
    (println "report:" report-out)
    report))
