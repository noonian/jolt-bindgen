(ns native-gen.clang
  "Drive clang: preprocess a header set, dump its AST as JSON, and have it
  parse small C probes, whose constants it folds without anything being run."
  (:require [native-gen.json :as json]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn- run [& args]
  (let [{:keys [exit out err]} (apply sh/sh args)]
    (when-not (zero? exit)
      (throw (ex-info (str "command failed: " (first args) " (exit " exit ")")
                      {:args args :exit exit :err err})))
    out))

(defn- flags [{:keys [include-dirs defines]}]
  (concat (map #(str "-I" %) include-dirs)
          (map #(str "-D" %) defines)))

(defn entry-source
  "C source that includes every header in the manifest."
  [{:keys [headers]}]
  (str/join "\n" (concat (map #(str "#include <" % ">") headers) [""])))

(defn preprocess
  "Preprocess the manifest's headers, keeping #define directives and line
  markers in the output. Writes <work-dir>/entry.c and returns the preprocessed
  text."
  [{:keys [work-dir] :as manifest}]
  (let [entry (io/file work-dir "entry.c")]
    (io/make-parents entry)
    (spit entry (entry-source manifest))
    (apply run "clang" "-E" "-dD" (concat (flags manifest) [(str entry)]))))

(defn ast-json
  "Dump the AST of an already-preprocessed translation unit as JSON text."
  [{:keys [work-dir] :as manifest} preprocessed]
  (let [pre (io/file work-dir "pre.c")]
    (spit pre preprocessed)
    (apply run "clang" "-Xclang" "-ast-dump=json" "-fsyntax-only" "-w"
           (concat (flags manifest) [(str pre)]))))

(def probe-prefix
  "What every declaration in a probe is named with, so its dump can be
  narrowed to them."
  "nativegen_probe_")

(defn probe-ast
  "Parse a probe source, without compiling or running it, and dump the
  declarations named with `probe-prefix`. Returns {:decls [node ...] :errors
  [{:line n :message s} ...]}: clang dumps what it could parse beside the
  errors it reports, each of which is on a line of the probe. An error
  anywhere else is the headers' own, and is thrown."
  [manifest c-file]
  (let [{:keys [exit out err]} (apply sh/sh "clang" "-Xclang" "-ast-dump=json"
                                      "-Xclang" (str "-ast-dump-filter=" probe-prefix)
                                      "-fsyntax-only" "-w" "-ferror-limit=0"
                                      (concat (flags manifest) [c-file]))
        errors (keep #(when-let [[_ file line msg] (re-find #"^(.*?):(\d+):\d+: (?:fatal )?error: (.*)$" %)]
                        {:file file :line (Long/parseLong line) :message msg})
                     (str/split-lines err))]
    (when (or (some #(not= c-file (:file %)) errors)
              (and (not (zero? exit)) (empty? errors)))
      (throw (ex-info "probe failed to parse" {:file c-file :exit exit :err err})))
    {:decls (json/parse-all out)
     :errors (mapv #(dissoc % :file) errors)}))

(defn probe-ir
  "The LLVM IR clang generates for a probe source, as text. Nothing is linked
  or run; the IR is where a floating-point constant appears folded."
  [manifest c-file]
  (apply run "clang" "-S" "-emit-llvm" "-w" "-o" "-" (concat (flags manifest) [c-file])))
