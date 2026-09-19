(ns native-gen.ast
  "Turn clang's JSON AST dump into a compact intermediate representation:
  typedefs, records, enums and functions, each attributed to the header that
  declared it."
  (:require [native-gen.json :as json]
            [clojure.string :as str]))

;; --- file attribution --------------------------------------------------
;;
;; clang prints a location's file only when it differs from the previously
;; printed one, and the same for the presumed file (the one named by a line
;; marker in preprocessed input). The reader's hook tracks that state in
;; document order and stamps every object that carries a "loc" with "_file".

(defn parse-dump [json-text]
  (let [current (volatile! nil)]
    (json/parse json-text
                {:on-pair (fn [parent k v]
                            (when (and (string? v)
                                       (not= parent "includedFrom")
                                       (or (= k "file") (= k "presumedFile")))
                              (vreset! current v))
                            (when (= k "loc")
                              [["_file" @current]]))})))

;; --- extraction --------------------------------------------------------

(defn- type-of [node]
  (let [t (get node "type")]
    {:type (get t "qualType")
     :desugared (get t "desugaredQualType" (get t "qualType"))}))

(defn- enum-constants [node]
  (loop [cs (get node "inner") prev -1 acc []]
    (if-let [c (first cs)]
      (if (= "EnumConstantDecl" (get c "kind"))
        (let [explicit (some-> c (get "inner") first (get "value"))
              v (if explicit (Long/parseLong explicit) (inc prev))]
          (recur (rest cs) v (conj acc [(get c "name") v])))
        (recur (rest cs) prev acc))
      acc)))

(declare record-ir)

(defn- record-fields [node]
  (loop [xs (get node "inner") pending nil acc []]
    (if-let [x (first xs)]
      (case (get x "kind")
        "RecordDecl" (if (get x "name")
                       (recur (rest xs) pending acc)
                       (recur (rest xs) (record-ir x) acc))
        "FieldDecl" (let [{:keys [type desugared]} (type-of x)]
                      (if (and pending (str/includes? type "unnamed"))
                        (recur (rest xs) nil (conj acc {:name (get x "name") :record pending}))
                        (recur (rest xs) pending
                               (conj acc {:name (get x "name") :type type :desugared desugared
                                          :bitfield? (boolean (get x "isBitfield"))}))))
        (recur (rest xs) pending acc))
      acc)))

(defn- record-ir [node]
  {:name (get node "name")
   :kind (keyword (get node "tagUsed"))
   :complete? (boolean (get node "completeDefinition"))
   :fields (record-fields node)
   :file (get node "_file")})

(defn- function-ir [node]
  (let [sig (get-in node ["type" "qualType"])
        ;; the return type is everything before the parameter list, which is
        ;; the last balanced parenthesised group in the signature
        params-start (loop [i (dec (count sig)) depth 0]
                       (let [c (nth sig i)]
                         (cond
                           (= c \)) (recur (dec i) (inc depth))
                           (= c \() (if (= depth 1) i (recur (dec i) (dec depth)))
                           :else (recur (dec i) depth))))]
    {:name (get node "name")
     :ret (str/trim (subs sig 0 params-start))
     :params (->> (get node "inner")
                  (filter #(= "ParmVarDecl" (get % "kind")))
                  (mapv #(assoc (type-of %) :name (get % "name"))))
     :variadic? (boolean (get node "variadic"))
     :file (get node "_file")}))

(defn extract
  "Return {:typedefs {name ir} :records {name ir} :enums {name ir}
  :functions [ir ...]} for the top-level declarations whose file satisfies
  `file-pred`. Records are merged by name so a forward declaration does not
  hide the later complete definition. An anonymous enum takes the name of
  the typedef that follows it (`typedef enum {...} Name;`), which is how
  clang itself refers to it."
  [dump file-pred]
  (dissoc
   (reduce
    (fn [ir node]
      (let [file (get node "_file")]
        (if-not (and file (file-pred file))
          ir
          (let [pending (::anonymous-enum ir)
                ir (dissoc ir ::anonymous-enum)]
            (case (get node "kind")
              "TypedefDecl" (let [{:keys [type] :as t} (type-of node)
                                  ir (assoc-in ir [:typedefs (get node "name")] (assoc t :file file))
                                  [_ enum-name] (re-matches #"enum (\S+)" (or type ""))]
                              (if (and pending enum-name (not (get-in ir [:enums enum-name])))
                                (assoc-in ir [:enums enum-name] (assoc pending :name enum-name))
                                ir))
              "RecordDecl" (let [r (record-ir node)
                                 name (:name r)]
                             (if (and name (or (:complete? r)
                                               (not (get-in ir [:records name]))))
                               (assoc-in ir [:records name] r)
                               ir))
              "EnumDecl" (if-let [name (get node "name")]
                           (assoc-in ir [:enums name] {:name name
                                                       :constants (enum-constants node)
                                                       :file file})
                           (assoc ir ::anonymous-enum {:constants (enum-constants node) :file file}))
              "FunctionDecl" (update ir :functions conj (function-ir node))
              ir)))))
    {:typedefs {} :records {} :enums {} :functions []}
    (get dump "inner"))
   ::anonymous-enum))
