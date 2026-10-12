(ns native-gen.emit
  "Render what was extracted as jolt source, one namespace per header: each
  target's view becomes a list of entries, the lists are merged, and where
  targets define a name differently the definition is chosen at load time. C
  identifiers are kept verbatim so generated names line up with the library's
  own documentation; an ergonomic layer on top chooses Clojure names."
  (:require [native-gen.types :as types]
            [clojure.string :as str]))

(defn header-ns
  "SDL3/SDL_video.h with prefix sdl3.ffi and strip SDL_ -> sdl3.ffi.video.
  :ns-names {\"joltc.h\" \"joltc.ffi\"} names a header's namespace outright."
  [{:keys [ns-prefix strip-prefix ns-names]} file]
  (let [header (last (str/split file #"/"))]
    (or (get ns-names header)
        (let [base (str/replace header #"\.h$" "")
              base (if (and strip-prefix (str/starts-with? base strip-prefix))
                     (subs base (count strip-prefix))
                     base)]
          (str ns-prefix "." (-> base str/lower-case (str/replace "_" "-")))))))

(defn ns-file [ns-sym]
  (str (-> (str ns-sym) (str/replace "." "/") (str/replace "-" "_")) ".jolt"))

(defn- long-literal [s]
  ;; a value outside a 64-bit long gets the bignum suffix
  (let [negative? (str/starts-with? s "-")
        digits (if negative? (subs s 1) s)
        limit (if negative? "9223372036854775808" "9223372036854775807")]
    (if (or (> (count digits) 19)
            (and (= 19 (count digits)) (pos? (compare digits limit))))
      (str s "N")
      s)))

(defn- constant-form [name {:keys [type value]}]
  (str "(def " name " "
       (case type
         (:int :uint) (long-literal value)
         :double (pr-str (double value))
         :string (pr-str value)
         :bool (pr-str value))
       ")"))

(defn- function-entry [ctx blocking {:keys [name ret params variadic?]}]
  (let [blocking? (contains? blocking name)
        raw-args (mapv #(types/param-type ctx (:type %)) params)
        ;; a blocking call releases the collector while it waits, so Chez
        ;; refuses to marshal a string for it: the caller passes a C string
        string->pointer? (and blocking? (some #{:string} raw-args))
        arg-types (if string->pointer? (mapv #(if (= :string %) :pointer %) raw-args) raw-args)
        ;; the bare marker: each call's tail is read off the values it is given
        arg-types (if variadic? (conj arg-types :&) arg-types)
        ret-type (types/return-type ctx ret)
        proto (str ret " " name "("
                   (str/join ", " (concat (map #(str (:type %) " " (:name %)) params)
                                          (when variadic? ["..."])))
                   ")")
        by-value-ret? (vector? ret-type)]
    {:section :functions
     :name name
     :comment (str/join
               "\n"
               (remove nil?
                       [(str ";; " proto)
                        (when by-value-ret?
                          ";; returns a struct by value: pass a pointer to receive it as the FIRST argument")
                        (when (some vector? arg-types)
                          ";; by-value struct arguments are passed as pointers to the struct")
                        (when string->pointer?
                          ";; blocking call: const char * arguments are passed as pointers (see ffi/with-c-string)")
                        (when variadic?
                          ";; variadic: arguments past the fixed ones travel per call as a 64-bit integer, a double or a C string")]))
     :form (str "(ffi/defcfn " name " " (pr-str name) " " (pr-str arg-types) " " (pr-str ret-type)
                (when blocking? " :blocking")
                ")")}))

(defn namespace-entries
  "What one target's view of a header defines, in the order it is written:
  {:entries [{:section k :name s :form s :comment s-or-nil} ...] :skipped
  [[name reason] ...]}. An entry is one definition, or for a record its
  descriptor and its layout together."
  [{:keys [ctx blocking]} {:keys [constants enums callbacks records functions]}]
  (let [taken (atom #{})
        claim! (fn [name] (if (@taken name) false (do (swap! taken conj name) true)))
        skipped (atom [])
        entries (atom [])
        add! (fn [section name form] (swap! entries conj {:section section :name name :form form}))]
    ;; functions and records claim their names first; a constant that
    ;; collides with either is reported rather than emitted
    (doseq [f functions] (claim! (:name f)))
    (doseq [r records] (claim! (:name r)) (claim! (str (:name r) "-desc")))
    (doseq [{:keys [name constants]} enums]
      ;; an enum the header never named has constants but no map
      (when (and name (claim! name))
        (add! :enums name (str "(def " name " {"
                               (str/join ", " (map (fn [[k v]] (str ":" k " " v)) constants))
                               "})")))
      (doseq [[k v] constants]
        (if (claim! k)
          (add! :enums k (str "(def " k " " v ")"))
          (swap! skipped conj [k "name already used"]))))
    (doseq [[name v] (sort-by key constants)]
      (if (claim! name)
        (add! :constants name (constant-form name v))
        (swap! skipped conj [name "name already used"])))
    (doseq [[name sig] (sort-by key callbacks)]
      (when (claim! name)
        (add! :callbacks name (str "(def " name " " (pr-str sig) ")"))))
    (doseq [r records]
      (let [desc (types/record-descriptor ctx r)]
        (add! :records (:name r) (str "(def " (:name r) "-desc " (pr-str desc) ")\n"
                                      "(def " (:name r) " (ffi/layout " (pr-str desc) "))"))))
    (doseq [f functions]
      (swap! entries conj (function-entry ctx blocking f)))
    {:entries @entries :skipped @skipped}))

(defn merge-entries
  "One namespace's entries from every target, {target entries}, as one list:
  [{:section k :name s :variants [{:targets [...] :form s :comment s} ...]}
  ...]. An entry has one variant when every target that defines it defines it
  the same way, and one per distinct form otherwise. Entries keep the order
  of the first target that has them."
  [targets by-target]
  (let [key-of (juxt :section :name)
        index (into {} (map (fn [t] [t (into {} (map (juxt key-of identity)) (get by-target t))])) targets)]
    (for [k (distinct (mapcat #(map key-of (get by-target %)) targets))
          :let [present (keep (fn [t] (when-let [e (get-in index [t k])] (assoc e :target t))) targets)
                by-form (group-by :form present)]]
      {:section (first k)
       :name (second k)
       :variants (mapv (fn [form]
                         (let [es (by-form form)]
                           {:targets (mapv :target es) :form form :comment (:comment (first es))}))
                       (distinct (map :form present)))})))

(defn divergence
  "The merged entries that are not one definition everywhere: {name {target
  form}} where targets define a name differently, and {name {:only
  [target ...]}} where some do not define it at all."
  [targets merged]
  (into {}
        (keep (fn [{:keys [name variants]}]
                (cond
                  (< 1 (count variants))
                  [name (into {} (for [{:keys [targets form]} variants, t targets] [t form]))]

                  (not= (count targets) (count (:targets (first variants))))
                  [name {:only (:targets (first variants))}])))
        merged))

(defn- entry-text
  "An entry as source. Where targets disagree, each form sits under the
  targets it is for, and a target that has none gets nil."
  [targets {:keys [variants]}]
  (let [{:keys [comment form]} (first variants)]
    (str (when comment (str comment "\n"))
         (if (= 1 (count variants))
           form
           (str "(case native-gen-target\n"
                (str/join "\n"
                          (for [{:keys [targets form]} variants]
                            (str "  "
                                 (if (= 1 (count targets))
                                   (pr-str (first targets))
                                   (str "(" (str/join " " (map pr-str targets)) ")"))
                                 " "
                                 (if (str/includes? form "\n")
                                   (str "(do " (str/replace form "\n" "\n      ") ")")
                                   form))))
                (when (not= (count targets) (count (mapcat :targets variants)))
                  "\n  nil")
                ")")))))

(defn- target-prelude
  "The definition every namespace of a binding made for several targets opens
  with: which of them it is being loaded on, and a refusal to load on any
  other, since a binding nobody extracted there is a guess at an ABI."
  [ns-sym targets]
  (str "\n(def native-gen-target\n"
       "  \"The target these bindings are loaded as. NATIVE_GEN_TARGET overrides the\n"
       "  detection: it loads them as one of the extracted targets on a platform that\n"
       "  is not among them, which nothing has checked.\"\n"
       "  (let [os (str (System/getProperty \"os.name\"))\n"
       "        arch (str (System/getProperty \"os.arch\"))\n"
       "        detected (str (cond (re-find #\"(?i)android\" os) \"android\"\n"
       "                            (re-find #\"(?i)mac|darwin\" os) \"darwin\"\n"
       "                            (re-find #\"(?i)win\" os) \"windows\"\n"
       "                            :else \"linux\")\n"
       "                      \"-\" (if (re-find #\"(?i)aarch64|arm64\" arch) \"arm64\" \"x86_64\"))\n"
       "        target (or (System/getenv \"NATIVE_GEN_TARGET\") detected)]\n"
       "    (when-not (contains? #{" (str/join " " (map pr-str targets)) "} target)\n"
       "      (throw (ex-info (str \"" ns-sym " has no bindings for \" target \": they were extracted for "
       (str/join ", " targets) ". Extract them there and regenerate, or set NATIVE_GEN_TARGET to one of those to load as it.\")\n"
       "                      {:target target :detected detected :extracted " (pr-str (vec targets)) "})))\n"
       "    target))"))

(def ^:private sections
  [[:enums "enums"]
   [:constants "constants"]
   [:callbacks "callback signatures, for ffi/callback"]
   [:records "structs and unions"]
   [:functions "functions"]])

(defn namespace-source
  "Source text for one header's namespace from its merged entries. A binding
  with one target reads as that target's definitions and nothing else."
  [{:keys [header-file source-label targets]} ns-sym merged]
  (str/join
   "\n"
   (concat
    [(str "(ns " ns-sym)
     (str "  \"Generated by native-gen from " header-file
          (when source-label (str " (" source-label ")"))
          ". Do not edit by hand.\"")
     "  (:require [jolt.ffi :as ffi]))"]
    (when (< 1 (count targets))
      [(target-prelude ns-sym targets)])
    (mapcat (fn [[section heading]]
              (let [es (filter #(= section (:section %)) merged)
                    ;; enums stay grouped as declared; the rest read by name
                    es (if (= :enums section) es (sort-by :name es))]
                (when (seq es)
                  (cons (str "\n;;; " heading) (map #(entry-text targets %) es)))))
            sections)
    [""])))

(defn render-namespace
  "Source text for one header's namespace as a single target defines it."
  [{:keys [header-file source-label] :as opts} ns-sym parts]
  (let [{:keys [entries skipped]} (namespace-entries opts parts)]
    {:source (namespace-source {:header-file header-file :source-label source-label :targets ["only"]}
                               ns-sym
                               (merge-entries ["only"] {"only" entries}))
     :skipped skipped}))
