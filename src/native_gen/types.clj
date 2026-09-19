(ns native-gen.types
  "Map C type strings from the AST onto jolt.ffi type keywords and layout
  descriptors."
  (:require [clojure.string :as str]))

(def base-types
  "C spellings that map straight to an ffi keyword. Fixed-width SDL typedefs are
  listed by name so they resolve before desugaring; `long` maps to the FFI's
  own C long, which is what the C side means on every platform."
  {"Uint8" :uint8 "Sint8" :int8 "Uint16" :uint16 "Sint16" :int16
   "Uint32" :uint32 "Sint32" :int32 "Uint64" :uint64 "Sint64" :int64
   "uint8_t" :uint8 "int8_t" :int8 "uint16_t" :uint16 "int16_t" :int16
   "uint32_t" :uint32 "int32_t" :int32 "uint64_t" :uint64 "int64_t" :int64
   "size_t" :size_t "ssize_t" :ssize_t "intptr_t" :iptr "uintptr_t" :uptr
   "int" :int "unsigned int" :uint "unsigned" :uint
   "short" :int16 "unsigned short" :uint16
   "long" :long "unsigned long" :ulong
   "long long" :int64 "unsigned long long" :uint64
   "char" :char "signed char" :int8 "unsigned char" :uint8
   "float" :float "double" :double
   "bool" :bool "_Bool" :bool
   "void" :void})

(defn- strip-qualifiers [s]
  (-> s
      (str/replace #"\b(const|volatile|restrict|__restrict)\b" "")
      (str/replace #"\s+" " ")
      str/trim))

(defn pointer-type? [s]
  (or (str/ends-with? s "*") (str/includes? s "(*")))

(defn- const-char-ptr? [s]
  (boolean (re-matches #"const char\s*\*" (str/trim s))))

(defn- array-dims
  "\"float[3][4]\" -> [\"float\" [3 4]], or nil."
  [s]
  (when-let [[_ elem dims] (re-matches #"(.*?)\s*((?:\[\d+\])+)" s)]
    [(str/trim elem) (mapv #(Long/parseLong %) (re-seq #"\d+" dims))]))

(declare resolve-type)

(defn- nest-array [t dims]
  (reduce (fn [inner n] [:array inner n]) t (reverse dims)))

(defn record-descriptor
  "The [:struct ...] or [:union ...] descriptor for a record ir, with nested
  records inlined. Throws when a field cannot be represented."
  [ctx record]
  [(:kind record)
   (mapv (fn [{:keys [name type record bitfield?] :as f}]
           (when bitfield?
             (throw (ex-info (str "bitfield " name " in " (:name record)) {:field f})))
           (when-not name
             (throw (ex-info (str "anonymous member in " (:name record)) {:field f})))
           [(keyword name)
            (if record
              (record-descriptor ctx record)
              (let [r (resolve-type ctx type)]
                (case (:kind r)
                  :scalar (:ffi r)
                  :pointer :pointer
                  :record (:descriptor r)
                  :array (nest-array (:elem r) (:dims r))
                  (throw (ex-info (str "unsupported field type " type " in " (:name record))
                                  {:field f :resolved r})))))])
         (:fields record))])

(defn try-descriptor
  "The record's descriptor, or nil when a field cannot be represented (a
  bitfield, an anonymous member, an unknown type); the reason is in the
  metadata's :reason when nil."
  [ctx record]
  (try (record-descriptor ctx record)
       (catch Exception e nil)))

(defn unrepresentable-reason
  "Why `try-descriptor` answers nil for a record, or nil when it does not."
  [ctx record]
  (try (record-descriptor ctx record) nil
       (catch Exception e (ex-message e))))

(defn resolve-type
  "Classify a C type string. Returns one of
    {:kind :scalar :ffi kw}
    {:kind :pointer :ffi :pointer|:string}
    {:kind :record :descriptor [...] :name s}
    {:kind :array :elem ffi-or-descriptor :dims [n ...]}
    {:kind :unsupported :reason s}
  `ctx` holds :typedefs, :records and :enums from native-gen.ast/extract."
  [ctx s]
  (let [raw (str/trim s)
        t (strip-qualifiers raw)]
    (cond
      (const-char-ptr? raw) {:kind :pointer :ffi :string}
      (pointer-type? t) {:kind :pointer :ffi :pointer}
      (array-dims t) (let [[elem dims] (array-dims t)
                           r (resolve-type ctx elem)]
                       (case (:kind r)
                         :scalar {:kind :array :elem (:ffi r) :dims dims}
                         :pointer {:kind :array :elem :pointer :dims dims}
                         :record {:kind :array :elem (:descriptor r) :dims dims}
                         {:kind :unsupported :reason (str "array of " elem)}))
      (contains? base-types t) {:kind :scalar :ffi (base-types t)}
      (str/starts-with? t "enum ") {:kind :scalar :ffi :int}
      (contains? (:enums ctx) t) {:kind :scalar :ffi :int}
      (or (str/starts-with? t "struct ") (str/starts-with? t "union "))
      (let [name (second (str/split t #" " 2))]
        (if-let [rec (get-in ctx [:records name])]
          (if (:complete? rec)
            (if-let [desc (try-descriptor ctx rec)]
              {:kind :record :name name :descriptor desc}
              {:kind :unsupported :reason (str t " by value: " (unrepresentable-reason ctx rec))})
            {:kind :unsupported :reason (str "opaque " t " by value")})
          {:kind :unsupported :reason (str "unknown " t)}))
      (contains? (:typedefs ctx) t)
      (resolve-type ctx (get-in ctx [:typedefs t :type]))
      (contains? (:records ctx) t)
      (resolve-type ctx (str (name (get-in ctx [:records t :kind])) " " t))
      :else {:kind :unsupported :reason (str "unknown type " t)})))

(defn param-type
  "The ffi argument type for a parameter, or {:unsupported reason}."
  [ctx s]
  (let [r (resolve-type ctx s)]
    (case (:kind r)
      :scalar (if (= :void (:ffi r)) {:unsupported "void parameter"} (:ffi r))
      :pointer (:ffi r)
      :record [:by-value (:descriptor r)]
      :array :pointer
      {:unsupported (:reason r)})))

(defn return-type
  "The ffi return type, or {:unsupported reason}. A `char *` return stays a
  pointer because the caller owns it and must free it; only `const char *`
  is copied out as a string."
  [ctx s]
  (let [r (resolve-type ctx s)]
    (case (:kind r)
      :scalar (:ffi r)
      :pointer (:ffi r)
      :record [:by-value (:descriptor r)]
      {:unsupported (:reason r)})))

(defn callback-signature
  "For a function-pointer type string like \"_Bool (*)(void *, SDL_Event *)\",
  return {:args [...] :ret kw} or nil."
  [ctx s]
  (when-let [[_ ret params] (re-matches #"(.*?)\s*\(\*\)\s*\((.*)\)" (str/trim s))]
    (let [ps (if (= "void" (str/trim params)) [] (map str/trim (str/split params #",")))
          args (mapv #(param-type ctx %) ps)
          r (return-type ctx ret)]
      (when (and (every? keyword? args) (or (keyword? r) (vector? r)))
        {:args args :ret r}))))
