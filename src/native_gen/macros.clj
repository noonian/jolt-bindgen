(ns native-gen.macros
  "Object-like #define constants. Their names and bodies come from the
  preprocessor's -dD output; their values come from compiling and running a
  probe program, so the C compiler does the evaluation and reports each
  value's type category through _Generic. Macros that are not constant
  expressions fail to compile and are pruned from the probe."
  (:require [native-gen.clang :as clang]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn candidates
  "Object-like macros from -dD preprocessor output, attributed to the header
  that defined them: {name {:body s :file f}}. Keeps names matching one of
  `prefixes`, defined in a file satisfying `file-pred`, and drops bodies that
  cannot be constant expressions."
  [preprocessed {:keys [prefixes exclude file-pred]}]
  (loop [lines (str/split-lines preprocessed) file nil acc {}]
    (if-let [line (first lines)]
      (cond
        (str/starts-with? line "# ")
        (recur (rest lines) (or (second (re-find #"^# \d+ \"([^\"]+)\"" line)) file) acc)

        (str/starts-with? line "#define ")
        (let [[_ mname tail] (re-find #"^#define\s+([A-Za-z_][A-Za-z0-9_]*)(.*)$" line)
              body (some-> tail str/trim)
              function-like? (and tail (str/starts-with? tail "("))]
          (if (and mname (not function-like?)
                   file (file-pred file)
                   (some #(str/starts-with? mname %) prefixes)
                   (not (contains? exclude mname))
                   (seq body)
                   (not (str/ends-with? mname "_h_"))
                   (not (re-find #"[{};#]|__attribute__|__declspec|^__" body)))
            (recur (rest lines) file (assoc acc mname {:body body :file file}))
            (recur (rest lines) file acc)))

        :else (recur (rest lines) file acc))
      acc)))

(def ^:private probe-prelude
  "Every association of a C generic selection is type-checked even when it is
  not chosen, so the branches select a printing function and only the chosen
  one is applied to the value."
  "#include <stdio.h>
static void pf(const char *n, double v) { printf(\"d %s %.17g\\n\", n, v); }
static void ps(const char *n, const char *v) { printf(\"s %s %s\\n\", n, v); }
static void pb(const char *n, int v) { printf(\"b %s %d\\n\", n, v); }
static void pu(const char *n, unsigned long long v) { printf(\"u %s %llu\\n\", n, v); }
static void pi(const char *n, long long v) { printf(\"i %s %lld\\n\", n, v); }
#define P(name, expr) _Generic((expr), \\
  float: pf, double: pf, \\
  char *: ps, const char *: ps, \\
  _Bool: pb, \\
  unsigned long long: pu, unsigned long: pu, unsigned int: pu, \\
  default: pi)(name, (expr))
int main(void) {
")

(defn- probe-source [headers names]
  (str (str/join "\n" (map #(str "#include <" % ">") headers)) "\n"
       probe-prelude
       (str/join "\n" (map #(str "  P(\"" % "\", (" % "));") names))
       "\n  return 0;\n}\n"))

(defn- parse-line [line]
  (when-let [[_ kind name value] (re-find #"^([fdsbui]) (\S+) ?(.*)$" line)]
    [name (case kind
            "f" {:type :float :value (Double/parseDouble value)}
            "d" {:type :double :value (Double/parseDouble value)}
            "s" {:type :string :value value}
            "b" {:type :bool :value (= "1" value)}
            "u" {:type :uint :value value}
            "i" {:type :int :value value})]))

(defn evaluate
  "Compile a probe for the candidate macros, pruning any whose line fails to
  compile, then run it. Returns {:values {name {:type k :value v :file f}}
  :pruned {name reason}}."
  [{:keys [headers work-dir] :as manifest} candidates]
  (let [c-file (str (io/file work-dir "macro_probe.c"))
        bin (str (io/file work-dir "macro_probe"))
        header-lines (+ (count headers) (count (str/split-lines probe-prelude)))]
    (loop [names (vec (sort (keys candidates))) pruned {} round 0]
      (spit c-file (probe-source headers names))
      (if-let [errors (clang/compile-errors manifest c-file bin)]
        (let [bad (into {} (keep (fn [{:keys [line message]}]
                                   (let [idx (- line header-lines 1)]
                                     (when (< -1 idx (count names))
                                       [(nth names idx) message])))
                                 errors))]
          (when (or (empty? bad) (> round 25))
            (throw (ex-info "macro probe: could not attribute compile errors" {:errors errors :round round})))
          (recur (vec (remove bad names)) (merge pruned bad) (inc round)))
        (let [out (clang/compile-and-run manifest c-file bin)
              values (into {} (keep parse-line (str/split-lines out)))]
          {:values (into {} (map (fn [[n v]] [n (assoc v :file (get-in candidates [n :file]))]) values))
           :pruned pruned})))))
