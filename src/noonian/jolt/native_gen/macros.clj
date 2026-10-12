(ns noonian.jolt.native-gen.macros
  "Object-like #define constants. Their names and bodies come from the
  preprocessor's -dD output; their types and values come from probes the C
  compiler parses, so it does the evaluation, and nothing is run: the values
  are those of whatever target clang is parsing for. Macros that are not
  constant expressions are rejected by the compiler and pruned."
  (:require [noonian.jolt.native-gen.clang :as clang]
            [noonian.jolt.native-gen.ast :as ast]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn- rejection
  "Why a wanted macro's definition cannot be a constant, or nil when it can
  be tried."
  [mname tail body]
  (cond (str/starts-with? tail "(") "function-like"
        (empty? body) "no value"
        (str/ends-with? mname "_h_") "include guard"
        (re-find #"[{};#]|__attribute__|__declspec|^__" body) "not a constant expression"))

(defn scan
  "The macros in -dD preprocessor output that the manifest wants: those
  matching one of `prefixes`, defined in a file satisfying `file-pred` and
  not in `exclude`. Returns {:candidates {name {:body s :file f}} :skipped
  {name reason}}: the object-like ones whose body could be a constant
  expression, attributed to the header that defined them, and the rest with
  why they are not. A later definition of a name replaces an earlier one."
  [preprocessed {:keys [prefixes exclude file-pred]}]
  (loop [lines (str/split-lines preprocessed) file nil acc {:candidates {} :skipped {}}]
    (if-let [line (first lines)]
      (cond
        (str/starts-with? line "# ")
        (recur (rest lines) (or (second (re-find #"^# \d+ \"([^\"]+)\"" line)) file) acc)

        (str/starts-with? line "#define ")
        (let [[_ mname tail] (re-find #"^#define\s+([A-Za-z_][A-Za-z0-9_]*)(.*)$" line)
              wanted? (and mname
                           file (file-pred file)
                           (some #(str/starts-with? mname %) prefixes)
                           (not (contains? exclude mname)))
              body (some-> tail str/trim)
              why (when wanted? (rejection mname tail body))]
          (recur (rest lines) file
                 (cond
                   (not wanted?) acc
                   why (-> acc (update :candidates dissoc mname) (assoc-in [:skipped mname] why))
                   :else (-> acc (update :skipped dissoc mname)
                             (assoc-in [:candidates mname] {:body body :file file})))))

        :else (recur (rest lines) file acc))
      acc)))

(defn candidates
  "The :candidates of `scan`."
  [preprocessed opts]
  (:candidates (scan preprocessed opts)))

(defn- includes [headers]
  (str (str/join "\n" (map #(str "#include <" % ">") headers)) "\n"))

(defn- probe-name [i]
  (str clang/probe-prefix i))

(defn- settle
  "Write a probe with one line of C per name, (line-fn probe-name name), after
  the headers, and have clang parse it; drop each name whose line it rejects,
  with the compiler's message, and try again until what is left parses.
  Returns {:names [...] :decls {name node} :pruned {name message}}."
  [{:keys [headers] :as manifest} c-file line-fn names]
  (loop [names (vec names) pruned {} round 0]
    (if (empty? names)
      {:names [] :decls {} :pruned pruned}
      (let [_ (spit c-file (str (includes headers)
                                (str/join "\n" (map-indexed (fn [i n] (line-fn (probe-name i) n)) names))
                                "\n"))
            {:keys [decls errors]} (clang/probe-ast manifest c-file)
            ;; the first error on a line is the one that names the cause
            bad (into {} (keep (fn [{:keys [line message]}]
                                 (let [idx (- line (count headers) 1)]
                                   (when (< -1 idx (count names))
                                     [(nth names idx) message])))
                               (reverse errors)))]
        (cond
          (empty? errors)
          (let [by-probe (zipmap (map probe-name (range)) names)]
            {:names names :pruned pruned
             :decls (into {} (keep (fn [d] (when-let [n (by-probe (get d "name"))] [n d])) decls))})

          (or (empty? bad) (> round 25))
          (throw (ex-info "macro probe: could not attribute errors" {:errors errors :round round :file c-file}))

          :else (recur (vec (remove bad names)) (merge pruned bad) (inc round)))))))

(defn- c-type [decl]
  (let [t (get decl "type")]
    (get t "desugaredQualType" (get t "qualType"))))

(defn- kind
  "The kind of constant a macro of this C type becomes, or nil when jolt has
  none for it."
  [type]
  (cond
    (contains? #{"float" "double" "long double"} type) :double
    (contains? #{"char *" "const char *"} type) :string
    (contains? #{"_Bool" "bool"} type) :bool
    (str/ends-with? type "*") nil
    (str/starts-with? type "unsigned") :uint
    :else :int))

(defn- string-literal
  "The string literal a declaration is initialised with, as clang prints it,
  through the casts and parentheses around it; nil when it is anything else."
  [decl]
  (loop [n (first (get decl "inner"))]
    (case (get n "kind")
      "StringLiteral" (get n "value")
      ("ImplicitCastExpr" "ParenExpr" "CStyleCastExpr") (recur (first (get n "inner")))
      nil)))

(defn unescape
  "The text of a C string literal as clang prints it: in its quotes, with its
  escapes, and every character outside ASCII as the octal bytes of its UTF-8."
  [literal]
  (let [s (subs literal (inc (str/index-of literal "\"")) (dec (count literal)))
        n (count s)]
    (loop [i 0 bytes []]
      (if (>= i n)
        (String. (byte-array (map unchecked-byte bytes)) "UTF-8")
        (let [c (nth s i)]
          (if (not= c \\)
            (recur (inc i) (conj bytes (int c)))
            (let [e (nth s (inc i))]
              (cond
                (Character/isDigit e)
                (let [digits (re-find #"^[0-7]{1,3}" (subs s (inc i)))]
                  (recur (+ i 1 (count digits)) (conj bytes (Long/parseLong digits 8))))

                (= e \x)
                (let [digits (re-find #"^[0-9a-fA-F]+" (subs s (+ i 2)))]
                  (recur (+ i 2 (count digits)) (conj bytes (Long/parseLong digits 16))))

                :else
                (recur (+ i 2) (conj bytes (get {\n 10 \t 9 \r 13 \a 7 \b 8 \f 12 \v 11} e (int e))))))))))))

(defn ir-double
  "A double as LLVM IR prints it: in decimal when that is exact, and otherwise
  as the sixteen hex digits of its bits."
  [s]
  (if (str/starts-with? s "0x")
    (let [top (Character/digit (nth s 2) 16)
          low (Long/parseLong (str (bit-and top 7) (subs s 3)) 16)]
      (Double/longBitsToDouble (if (> top 7) (+ Long/MIN_VALUE low) low)))
    (Double/parseDouble s)))

(defn evaluate
  "Have clang evaluate the candidate macros, three probes that it parses and
  never runs: one declares a variable of each macro's own type, which gives
  the type and prunes whatever is not an expression; one makes each integer
  an enum constant, which clang folds; one makes each floating-point value a
  global, which is folded in the IR. Returns {:values {name {:type k :value
  v :file f}} :pruned {name reason}}."
  [{:keys [work-dir] :as manifest} candidates]
  (let [file #(str (io/file work-dir %))
        typed (settle manifest (file "macro_types.c")
                      #(str "__auto_type " %1 " = (" %2 ");")
                      (sort (keys candidates)))
        types (into {} (map (fn [[n d]] [n (c-type d)])) (:decls typed))
        of-kind (fn [kinds] (filter #(contains? kinds (some-> (types %) kind)) (:names typed)))
        ints (settle manifest (file "macro_values.c")
                     #(str "enum " %1 " { " %1 "_v = (" %2 ") };")
                     (of-kind #{:int :uint :bool}))
        doubles (settle manifest (file "macro_doubles.c")
                        #(str "const double " %1 " = (" %2 ");")
                        (of-kind #{:double}))
        folded (when (seq (:names doubles))
                 (into {} (map (fn [[_ i v]] [(nth (:names doubles) (Long/parseLong i)) (ir-double v)]))
                       (re-seq (re-pattern (str "(?m)^@" clang/probe-prefix "(\\d+) = .*?\\bdouble ([^,\\s]+)"))
                               (clang/probe-ir manifest (file "macro_doubles.c")))))
        ;; a constant, or the reason there is none
        constant (fn [n]
                   (let [type (types n)
                         k (some-> type kind)]
                     (case k
                       nil (if type (str "a constant of type " type) "not declared by the probe")
                       :string (if-let [literal (string-literal (get-in typed [:decls n]))]
                                 {:type :string :value (unescape literal)}
                                 "a string that is not a literal")
                       :double (if-let [v (get folded n)]
                                 {:type :double :value v}
                                 (get-in doubles [:pruned n] "not a floating-point constant"))
                       (if-let [v (some-> (get-in ints [:decls n]) ast/enum-values first)]
                         {:type k :value (if (= :bool k) (not (contains? #{"false" "0"} v)) v)}
                         (get-in ints [:pruned n] "not an integer constant")))))
        results (map (juxt identity constant) (:names typed))]
    {:values (into {} (keep (fn [[n r]] (when (map? r) [n (assoc r :file (get-in candidates [n :file]))])) results))
     :pruned (into (:pruned typed) (filter (comp string? second) results))}))
