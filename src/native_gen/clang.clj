(ns native-gen.clang
  "Drive clang: preprocess a header set, dump its AST as JSON, and compile and
  run small C probe programs."
  (:require [clojure.java.shell :as sh]
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

(defn compile-errors
  "Compile a C source file. Returns nil on success, or a vector of
  {:line n :message s} for each error clang reports."
  [manifest c-file out-file]
  (let [{:keys [exit err]} (apply sh/sh "clang" "-w" "-ferror-limit=0" "-o" out-file
                                  (concat (flags manifest) [c-file]))]
    (when-not (zero? exit)
      (->> (str/split-lines err)
           (keep #(when-let [[_ line msg] (re-find #":(\d+):\d+: error: (.*)$" %)]
                    {:line (Long/parseLong line) :message msg}))
           vec))))

(defn compile-and-run
  "Compile and run a C probe. Returns its stdout."
  [manifest c-file out-file & run-args]
  (when-let [errors (compile-errors manifest c-file out-file)]
    (throw (ex-info "probe failed to compile" {:errors errors :file c-file})))
  (apply run out-file run-args))
