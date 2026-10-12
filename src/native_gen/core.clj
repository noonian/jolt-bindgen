(ns native-gen.core
  "The generator driver, in two stages. Extraction runs clang for one target
  and records what it saw in a file of its own; emission merges every target
  on record into the binding namespaces, the layout test and a report, and
  needs no compiler. `generate!` does both from a manifest map."
  (:require [native-gen.clang :as clang]
            [native-gen.ast :as ast]
            [native-gen.types :as types]
            [native-gen.macros :as macros]
            [native-gen.emit :as emit]
            [native-gen.verify :as verify]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; --- targets -------------------------------------------------------------

(def triples
  "The targets a binding can be extracted for, and what clang calls each."
  {"darwin-arm64" "arm64-apple-macosx"
   "darwin-x86_64" "x86_64-apple-macosx"
   "linux-x86_64" "x86_64-unknown-linux-gnu"
   "linux-arm64" "aarch64-unknown-linux-gnu"
   "windows-x86_64" "x86_64-pc-windows-msvc"
   "android-arm64" "aarch64-linux-android"})

(defn os-key []
  (let [n (str/lower-case (or (System/getProperty "os.name") ""))]
    (cond (str/includes? n "mac") :darwin
          (str/includes? n "win") :windows
          :else :linux)))

(defn host-target
  "The target this machine is, named as the generated bindings name it."
  []
  (str (name (os-key)) "-"
       (if (re-find #"(?i)aarch64|arm64" (or (System/getProperty "os.arch") "")) "arm64" "x86_64")))

(defn- per-target
  "A manifest value that may be given per target, or per operating system."
  [v target]
  (if (map? v)
    (or (get v target) (get v (keyword (first (str/split target #"-")))))
    v))

(defn load-manifest [path]
  (let [m (edn/read-string (slurp path))
        dir (or (.getParent (io/file path)) ".")
        project-dir (str (io/file dir (or (:project-dir m) "..")))]
    (-> m
        (assoc :manifest-dir dir :project-dir project-dir)
        (update :work-dir #(str (io/file dir (or % "work"))))
        (update :targets-dir #(str (io/file dir (or % "targets"))))
        (update :out-dir #(str (io/file project-dir (or % "src"))))
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

;; --- extraction ----------------------------------------------------------

(defn- header-path
  "A declaration's file as the header is included, not as this machine finds
  it: the path under whichever include directory holds it."
  [include-dirs file]
  (if-let [dir (->> include-dirs
                    (map #(if (str/ends-with? % "/") % (str % "/")))
                    (filter #(str/starts-with? file %))
                    (sort-by count)
                    last)]
    (subs file (count dir))
    (last (str/split file #"/"))))

(defn- update-files [f x]
  (cond
    (map? x) (into {} (map (fn [[k v]] [k (if (and (= :file k) (string? v)) (f v) (update-files f v))])) x)
    (vector? x) (mapv #(update-files f %) x)
    :else x))

(defn- type-names
  "Every identifier in the type spellings of what was extracted."
  [x]
  (cond
    (map? x) (mapcat (fn [[k v]] (if (and (#{:type :desugared :ret} k) (string? v))
                                   (re-seq #"[A-Za-z_]\w*" v)
                                   (type-names v)))
                     x)
    (vector? x) (mapcat type-names x)
    :else nil))

(defn extract
  "Run clang for one target and return what it saw of the manifest's headers:
  the declarations as clang spells them, the typedefs from other headers
  that they use, the macros and the layouts it folded. The host is parsed as
  itself; any other target is parsed for its triple, :cross? true, which only
  headers that need no system header of the target's survive."
  [{:keys [macro-prefixes macro-exclude source-label] :as manifest} target]
  (let [host? (= target (host-target))
        _ (when-not (or host? (triples target))
            (throw (ex-info (str "unknown target " target) {:known (sort (keys triples))})))
        _ (when (and (map? (:include-dirs manifest)) (nil? (per-target (:include-dirs manifest) target)))
            (throw (ex-info (str "the manifest's :include-dirs has no entry for " target) {})))
        m (assoc manifest
                 :include-dirs (per-target (:include-dirs manifest) target)
                 :work-dir (str (io/file (:work-dir manifest) target))
                 :clang-target (when-not host? (triples target)))
        pred (file-pred m)
        pre (clang/preprocess m)
        ir (ast/extract (ast/parse-dump (clang/ast-json m pre)) pred)
        scanned (macros/scan pre {:prefixes macro-prefixes :exclude macro-exclude :file-pred pred})
        {:keys [values pruned]} (macros/evaluate m (:candidates scanned))
        ;; a record with a bitfield or an anonymous member has no jolt
        ;; layout, so there is nothing of it to measure
        records (filter #(types/try-descriptor ir %) (filter :complete? (vals (:records ir))))
        used (set (type-names (dissoc ir :system-typedefs)))]
    (update-files
     #(header-path (:include-dirs m) %)
     (assoc ir
            :target target
            :cross? (not host?)
            :source-label source-label
            :functions (vec (sort-by :name (:functions ir)))
            ;; what another header calls a type differs by platform; what it
            ;; comes to is all a binding needs
            :system-typedefs (into {} (keep (fn [[n t]] (when (used n) [n (select-keys t [:desugared])])))
                                   (:system-typedefs ir))
            :macros values
            :pruned-macros pruned
            :skipped-macros (:skipped scanned)
            :layouts (verify/measure m ir records)))))

;; --- targets on record ---------------------------------------------------

(defn- sorted [x]
  (cond
    (map? x) (into (sorted-map) (map (fn [[k v]] [k (sorted v)])) x)
    (sequential? x) (mapv sorted x)
    :else x))

(defn target-text
  "A target as it is recorded: EDN, one declaration to a line and every map
  in key order, so that a change to a header reads as a change to its lines."
  [target]
  (str "{"
       (str/join "\n "
                 (for [[k v] (sorted target)]
                   (cond
                     (and (map? v) (seq v))
                     (str k "\n {" (str/join "\n  " (map (fn [[n x]] (str (pr-str n) " " (pr-str x))) v)) "}")

                     (and (vector? v) (seq v))
                     (str k "\n [" (str/join "\n  " (map pr-str v)) "]")

                     :else (str k " " (pr-str v)))))
       "}\n"))

(defn target-file [{:keys [targets-dir]} target]
  (io/file targets-dir (str target ".edn")))

(defn read-targets
  "Every target on record, {name target}."
  [{:keys [targets-dir]}]
  (into (sorted-map)
        (keep (fn [f] (when (str/ends-with? (.getName f) ".edn")
                        (let [t (edn/read-string (slurp f))]
                          [(:target t) t]))))
        (.listFiles (io/file targets-dir))))

(defn- write! [path text]
  (io/make-parents path)
  (spit path text)
  (str path))

(defn- failure
  "Why clang could not parse a target's headers, in a line."
  [e]
  (or (some #(when (re-find #"error:" %) (str/trim %))
            (str/split-lines (str (:err (ex-data e)))))
      (ex-message e)))

(defn extract!
  "Extract the host, and whichever of the manifest's :targets can be parsed
  from here, into the targets directory. A target recorded on its own
  platform is not replaced by a view of it from this one. Returns {target
  reason} for the targets that could not be extracted here."
  [manifest]
  (let [host (host-target)]
    (into {}
          (keep (fn [target]
                  (let [f (target-file manifest target)
                        recorded (when (.exists f) (edn/read-string (slurp f)))]
                    (cond
                      (= target host)
                      (do (println "extracting" target)
                          (write! f (target-text (extract manifest target)))
                          nil)

                      (and recorded (not (:cross? recorded)))
                      (do (println "keeping" target "as extracted there")
                          nil)

                      :else
                      (try
                        (println "extracting" target "from" host)
                        (write! f (target-text (extract manifest target)))
                        nil
                        (catch Exception e
                          [target (failure e)]))))))
          (distinct (cons host (:targets manifest))))))

(defn check
  "Extract the host afresh and compare it with what is on record for it, which
  may have been extracted from elsewhere. Returns nil when they agree, else
  {:target name :differs {section [name ...]}}, and {:differs :all} when
  nothing is on record."
  [manifest]
  (let [host (host-target)
        f (target-file manifest host)
        fresh (dissoc (edn/read-string (target-text (extract manifest host))) :cross?)
        recorded (when (.exists f) (dissoc (edn/read-string (slurp f)) :cross?))
        by-name (fn [v] (if (map? v) v (into {} (map (juxt #(or (:name %) %) identity)) v)))]
    (cond
      (nil? recorded) {:target host :differs :all}
      (not= fresh recorded)
      {:target host
       :differs (into (sorted-map)
                      (keep (fn [k]
                              (let [a (get fresh k) b (get recorded k)]
                                (when (not= a b)
                                  [k (if (and (coll? a) (coll? b))
                                       (let [a (by-name a) b (by-name b)]
                                         (vec (sort (filter #(not= (get a %) (get b %))
                                                            (distinct (concat (keys a) (keys b)))))))
                                       [b a])]))))
                      (distinct (concat (keys fresh) (keys recorded))))})))

;; --- emission ------------------------------------------------------------

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

(defn- target-view
  "One target as the binding would define it alone: {:namespaces {ns {:file
  header :entries [...] :enums n}}} and what it leaves out, with the reason."
  [{:keys [blocking] :as manifest} t]
  (let [ctx t
        classified (map #(classify-function ctx blocking %) (:functions t))
        functions (map second (filter #(= :ok (first %)) classified))
        callback-verdicts (into {} (keep (fn [[n td]] (when-let [sig (types/callback-signature ctx (:type td))]
                                                        [n (assoc sig :file (:file td))]))
                                       (:typedefs t)))
        callbacks (into {} (remove (comp :unsupported val) callback-verdicts))
        enums (concat (sort-by :name (vals (:enums t))) (:anonymous-enums t))
        complete (filter :complete? (vals (:records t)))
        ;; a record with a bitfield or an anonymous member has no jolt
        ;; layout; it is reported and left out, and pointers to it still work
        records (filter #(types/try-descriptor ctx %) complete)
        values (:macros t)
        files (set (concat (map :file functions) (map :file records) (map :file enums)
                           (map :file (vals values)) (map :file (vals callbacks))))
        namespaces (into {}
                         (for [file files
                               :let [in-file #(= file (:file %))
                                     parts {:functions (sort-by :name (filter in-file functions))
                                            :records (sort-by :name (filter in-file records))
                                            :enums (filter in-file enums)
                                            :constants (into {} (filter (comp in-file val) values))
                                            :callbacks (into {} (map (fn [[n s]] [n (dissoc s :file)])
                                                                     (filter (comp in-file val) callbacks)))}]]
                           [(symbol (emit/header-ns manifest file))
                            (assoc (emit/namespace-entries {:ctx ctx :blocking blocking} parts)
                                   :file (last (str/split file #"/"))
                                   :enums (count (:enums parts)))]))]
    {:namespaces namespaces
     :skipped-functions (into {} (map (fn [[_ f why]] [(:name f) why]) (remove #(= :ok (first %)) classified)))
     :skipped-records (into {} (keep (fn [r] (when-let [why (types/unrepresentable-reason ctx r)] [(:name r) why]))) complete)
     :skipped-callbacks (into {} (keep (fn [[n sig]] (when-let [why (:unsupported sig)] [n why])) callback-verdicts))
     ;; a global is data in the library, not a function to bind
     :skipped-variables (into {} (map (fn [v] [(:name v) (str "global variable of type " (:type v))]) (:variables t)))
     :skipped-macros (:skipped-macros t)
     :pruned-macros (:pruned-macros t)
     :skipped-names (into {} (mapcat :skipped (vals namespaces)))
     :opaque-records (map :name (remove :complete? (vals (:records t))))}))

(defn emit!
  "Write the binding namespaces, the layout test and the report from the
  targets on record, {name target}. Returns the report."
  [{:keys [out-dir test-out test-ns report-out source-label] :as manifest} targets]
  (let [views (into (sorted-map) (map (fn [[id t]] [id (target-view manifest t)])) targets)
        ids (vec (keys views))
        across (fn [k] (into (sorted-map) (mapcat k (vals views))))
        emitted (for [ns-sym (sort (distinct (mapcat (comp keys :namespaces) (vals views))))
                      :let [of (fn [k] (keep #(get-in % [:namespaces ns-sym k]) (vals views)))
                            merged (emit/merge-entries
                                    ids (into {} (map (fn [[id v]] [id (get-in v [:namespaces ns-sym :entries])])) views))
                            source (emit/namespace-source {:header-file (first (of :file))
                                                           :source-label source-label
                                                           :targets ids}
                                                          ns-sym merged)
                            in-section (fn [section] (filter #(= section (:section %)) merged))]]
                  (do (write! (str (io/file out-dir (emit/ns-file ns-sym))) source)
                      {:ns ns-sym
                       :counts {:functions (count (in-section :functions))
                                :records (count (in-section :records))
                                :enums (apply max (of :enums))
                                :constants (count (in-section :constants))
                                :callbacks (count (in-section :callbacks))}
                       :records (sort (map :name (in-section :records)))
                       :divergent (emit/divergence ids merged)}))
        ns->records (into {} (keep (fn [{:keys [ns records]}] (when (seq records) [ns records])) emitted))
        test-path (-> (str (io/file test-out (emit/ns-file (symbol test-ns))))
                      (str/replace #"\.jolt$" ".clj"))
        report {:targets (into (sorted-map)
                               (map (fn [[id t]] [id (select-keys t [:cross? :source-label])]))
                               targets)
                :namespaces (mapv #(select-keys % [:ns :counts]) emitted)
                :divergent (into (sorted-map)
                                 (keep (fn [{:keys [ns divergent]}] (when (seq divergent) [ns (into (sorted-map) divergent)])))
                                 emitted)
                :skipped-functions (across :skipped-functions)
                :skipped-records (across :skipped-records)
                :skipped-callbacks (across :skipped-callbacks)
                :skipped-variables (across :skipped-variables)
                :skipped-macros (across :skipped-macros)
                :pruned-macros (across :pruned-macros)
                :skipped-names (across :skipped-names)
                :opaque-records (vec (sort (distinct (mapcat :opaque-records (vals views)))))}]
    (write! test-path (verify/test-source test-ns
                                          (into (sorted-map) (map (fn [[id t]] [id (:layouts t)])) targets)
                                          ns->records))
    (println "targets" (str/join ", " ids) "| namespaces" (count emitted)
             "| functions" (reduce + (map (comp :functions :counts) emitted))
             "skipped" (count (:skipped-functions report))
             "| records" (reduce + (map (comp :records :counts) emitted))
             "skipped" (count (:skipped-records report))
             "| divergent" (reduce + (map (comp count :divergent) emitted)))
    report))

(defn generate!
  "Extract what can be extracted here, then emit from every target on record.
  Returns the report, which is also written."
  [{:keys [report-out] :as manifest}]
  (let [not-extracted (extract! manifest)
        report (cond-> (emit! manifest (read-targets manifest))
                 (seq not-extracted) (assoc :not-extracted not-extracted))]
    (doseq [[target why] not-extracted]
      (println "not extracted here:" target "-" why))
    (write! report-out (with-out-str (clojure.pprint/pprint report)))
    (println "report:" report-out)
    report))
