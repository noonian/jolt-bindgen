(ns native-gen.json
  "A small JSON reader, enough for clang's AST dump.

  Objects become maps with string keys, arrays become vectors, numbers become
  longs or doubles, and null becomes nil. Key order inside an object is not
  preserved by the maps, so the reader offers one document-order hook: an
  `:on-pair` function called as (on-pair parent-key key value) after every
  object pair is read, in reading order. clang's dump omits a location's file
  name when it equals the previously printed one, so a consumer that needs
  file attribution tracks that state through the hook.")

(defn- fail [s ^long i msg]
  (throw (ex-info (str "json: " msg " at " i)
                  {:index i :near (subs s (max 0 (- i 20)) (min (count s) (+ i 20)))})))

(defn- ws? [c]
  (or (identical? c \space) (identical? c \newline) (identical? c \tab) (identical? c \return)))

(defn- skip-ws ^long [^String s ^long i]
  (let [n (count s)]
    (loop [i i]
      (if (and (< i n) (ws? (nth s i))) (recur (inc i)) i))))

(defn- hex-val ^long [c]
  (let [v (Character/digit c 16)]
    (if (neg? v) (throw (ex-info "json: bad hex digit" {:char c})) v)))

(defn- read-string* [^String s ^long i]
  ;; i points at the opening quote. Returns [string next-index].
  (let [n (count s)
        sb (StringBuilder.)]
    (loop [i (inc i)]
      (when (>= i n) (fail s i "unterminated string"))
      (let [c (nth s i)]
        (cond
          (identical? c \") [(str sb) (inc i)]
          (identical? c \\)
          (let [e (nth s (inc i))]
            (case e
              \" (do (.append sb \") (recur (+ i 2)))
              \\ (do (.append sb \\) (recur (+ i 2)))
              \/ (do (.append sb \/) (recur (+ i 2)))
              \b (do (.append sb \backspace) (recur (+ i 2)))
              \f (do (.append sb \formfeed) (recur (+ i 2)))
              \n (do (.append sb \newline) (recur (+ i 2)))
              \r (do (.append sb \return) (recur (+ i 2)))
              \t (do (.append sb \tab) (recur (+ i 2)))
              \u (let [cp (+ (* 4096 (hex-val (nth s (+ i 2))))
                             (* 256 (hex-val (nth s (+ i 3))))
                             (* 16 (hex-val (nth s (+ i 4))))
                             (hex-val (nth s (+ i 5))))]
                   (.append sb (char cp))
                   (recur (+ i 6)))
              (fail s i (str "bad escape \\" e))))
          :else (do (.append sb c) (recur (inc i))))))))

(defn- number-char? [c]
  (or (Character/isDigit c) (identical? c \-) (identical? c \+) (identical? c \.)
      (identical? c \e) (identical? c \E)))

(defn- read-number [^String s ^long i]
  (let [n (count s)
        j (loop [j i] (if (and (< j n) (number-char? (nth s j))) (recur (inc j)) j))
        tok (subs s i j)]
    [(if (re-find #"[.eE]" tok) (Double/parseDouble tok) (Long/parseLong tok)) j]))

(defn- expect-word [^String s ^long i ^String w v]
  (if (and (<= (+ i (count w)) (count s)) (= w (subs s i (+ i (count w)))))
    [v (+ i (count w))]
    (fail s i (str "expected " w))))

(declare read-value)

(defn- read-array [^String s ^long i hook parent-key]
  (loop [i (skip-ws s (inc i)) acc (transient [])]
    (let [c (nth s i)]
      (if (identical? c \])
        [(persistent! acc) (inc i)]
        (let [[v j] (read-value s i hook parent-key)
              j (skip-ws s j)
              c (nth s j)]
          (cond
            (identical? c \,) (recur (skip-ws s (inc j)) (conj! acc v))
            (identical? c \]) [(persistent! (conj! acc v)) (inc j)]
            :else (fail s j "expected , or ] in array")))))))

(defn- read-object [^String s ^long i hook parent-key]
  (loop [i (skip-ws s (inc i)) acc (transient {})]
    (let [c (nth s i)]
      (if (identical? c \})
        [(persistent! acc) (inc i)]
        (let [_ (when-not (identical? c \") (fail s i "expected key"))
              [k j] (read-string* s i)
              j (skip-ws s j)
              _ (when-not (identical? (nth s j) \:) (fail s j "expected :"))
              [v j] (read-value s (skip-ws s (inc j)) hook k)
              acc (conj! acc [k v])
              acc (if hook
                    (let [extra (hook parent-key k v)]
                      (if extra (reduce conj! acc extra) acc))
                    acc)
              j (skip-ws s j)
              c (nth s j)]
          (cond
            (identical? c \,) (recur (skip-ws s (inc j)) acc)
            (identical? c \}) [(persistent! acc) (inc j)]
            :else (fail s j "expected , or } in object")))))))

(defn- read-value [^String s ^long i hook parent-key]
  (let [c (nth s i)]
    (cond
      (identical? c \{) (read-object s i hook parent-key)
      (identical? c \[) (read-array s i hook parent-key)
      (identical? c \") (read-string* s i)
      (identical? c \t) (expect-word s i "true" true)
      (identical? c \f) (expect-word s i "false" false)
      (identical? c \n) (expect-word s i "null" nil)
      (number-char? c) (read-number s i)
      :else (fail s i (str "unexpected character " c)))))

(defn parse
  "Parse JSON text. Options: `:on-pair`, a function of
  (parent-key key value) called after each object pair is read; it may return
  a seq of extra [key value] pairs to add to the object being read, or nil."
  ([s] (parse s nil))
  ([^String s {:keys [on-pair]}]
   (let [[v j] (read-value s (skip-ws s 0) on-pair nil)
         j (skip-ws s j)]
     (when (< j (count s)) (fail s j "trailing content"))
     v)))
