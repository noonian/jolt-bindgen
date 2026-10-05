(ns native-gen.types-test
  (:require [clojure.test :refer [deftest is testing]]
            [native-gen.types :as types]
            [native-gen.fixture :as fx]))

(def point [:struct [[:x :int32] [:y :int32]]])

(def ctx
  "A hand-built context, so the mapping rules are checked without clang."
  {:typedefs {"Handle" {:type "struct Handle *"}
              "Flags" {:type "uint32_t"}
              "Point" {:type "struct Point"}
              "Shape" {:type "union Shape"}
              "Seconds" {:type "time_t"}}
   :system-typedefs {"time_t" {:type "__darwin_time_t" :desugared "long"}
                     "va_list" {:type "__builtin_va_list" :desugared "char *"}}
   :records {"Point" {:name "Point" :kind :struct :complete? true
                      :fields [{:name "x" :type "int32_t"} {:name "y" :type "int32_t"}]}
             "Shape" {:name "Shape" :kind :union :complete? true
                      :fields [{:name "i" :type "int"} {:name "d" :type "double"}]}
             "Tagged" {:name "Tagged" :kind :struct :complete? true
                       :fields [{:name "kind" :type "int"} {:name "shape" :type "Shape"}]}
             "Handle" {:name "Handle" :kind :struct :complete? false :fields []}
             "Bits" {:name "Bits" :kind :struct :complete? true
                     :fields [{:name "a" :type "unsigned int" :bitfield? true}]}}
   :enums {"Mode" {:name "Mode" :constants [["A" 0]]}}})

(deftest scalars-and-pointers
  (is (= :int (types/param-type ctx "int")))
  (is (= :uint32 (types/param-type ctx "Flags")))
  (is (= :int (types/param-type ctx "Mode")))
  (is (= :int (types/param-type ctx "enum Mode")))
  (is (= :pointer (types/param-type ctx "Handle")))
  (is (= :pointer (types/param-type ctx "const struct Point *")))
  (is (= :pointer (types/param-type ctx "float[3]")))
  (is (= {:unsupported "void parameter"} (types/param-type ctx "void")))
  (is (= :void (types/return-type ctx "void")))
  (is (= {:unsupported "unknown type wchar_t"} (types/param-type ctx "wchar_t"))))

(deftest strings
  (is (= :string (types/param-type ctx "const char *")))
  (is (= :string (types/param-type ctx "char const *")))
  (is (= :string (types/param-type ctx "const char *const")))
  (is (= :pointer (types/param-type ctx "char *")))
  (testing "a char * return is owned by the caller, so it stays a pointer"
    (is (= :pointer (types/return-type ctx "char *")))
    (is (= :string (types/return-type ctx "const char *")))))

(deftest typedefs-from-other-headers
  (testing "resolve through the spelling clang desugars them to"
    (is (= :long (types/param-type ctx "time_t")))
    (is (= :long (types/return-type ctx "Seconds"))))
  (testing "except va_list, which no jolt type spells"
    (is (= {:unsupported "va_list"} (types/param-type ctx "va_list")))
    (is (= {:unsupported "va_list"} (types/param-type ctx "__builtin_va_list")))))

(deftest aggregates
  (is (= [:by-value point] (types/param-type ctx "Point")))
  (is (= [:by-value point] (types/return-type ctx "struct Point")))
  (is (= [:struct [[:kind :int] [:shape [:union [[:i :int] [:d :double]]]]]]
         (types/record-descriptor ctx (get-in ctx [:records "Tagged"]))))
  (testing "a union, alone or nested, has a layout but cannot cross by value"
    (is (= "union Shape by value holds a union: pass it by pointer"
           (:unsupported (types/param-type ctx "Shape"))))
    (is (= "struct Tagged by value holds a union: pass it by pointer"
           (:unsupported (types/return-type ctx "Tagged"))))
    (is (types/holds-union? [:struct [[:a [:array [:union [[:i :int]]] 2]]]]))
    (is (not (types/holds-union? [:struct [[:a [:array :int 2]]]]))))
  (testing "an opaque or unrepresentable record cannot cross by value"
    (is (= "opaque struct Handle by value" (:unsupported (types/param-type ctx "struct Handle"))))
    (is (= "struct Bits by value: bitfield a in Bits" (:unsupported (types/param-type ctx "Bits"))))
    (is (= "bitfield a in Bits" (types/unrepresentable-reason ctx (get-in ctx [:records "Bits"]))))
    (is (nil? (types/try-descriptor ctx (get-in ctx [:records "Bits"]))))))

(deftest callbacks
  (is (= {:args [:pointer :pointer] :ret :bool}
         (types/callback-signature ctx "_Bool (*)(void *, struct Point *)")))
  (is (= {:args [] :ret :void} (types/callback-signature ctx "void (*)(void)")))
  (is (= {:args [:int] :ret [:by-value point]} (types/callback-signature ctx "Point (*)(int)")))
  (is (nil? (types/callback-signature ctx "int")))
  (is (nil? (types/callback-signature ctx "void (*)(va_list)"))))

(deftest fixture-types
  (let [ctx @fx/ir]
    (is (contains? #{:long :int64} (types/return-type ctx "time_t")))
    (is (= :size_t (types/return-type ctx "size_t")))
    (is (= {:unsupported "va_list"} (types/param-type ctx "va_list")))
    (is (= [:by-value [:struct [[:origin [:struct [[:x :int32] [:y :int32]]]]
                                [:size [:array :float 2]]
                                [:tag :uint8]]]]
           (types/param-type ctx "FX_Rect")))
    (is (= [:struct [[:m [:array [:array :float 4] 3]]]]
           (types/record-descriptor ctx (get-in ctx [:records "FX_Matrix"]))))
    (is (= [:struct [[:kind :int] [:u [:union [[:i :int] [:f :float]]]]]]
           (types/record-descriptor ctx (get-in ctx [:records "FX_Named"]))))
    (is (= "anonymous member in FX_Anonymous"
           (types/unrepresentable-reason ctx (get-in ctx [:records "FX_Anonymous"]))))
    (is (:unsupported (types/param-type ctx "FX_Value")))
    (is (:unsupported (types/param-type ctx "FX_Tagged")))
    (is (= :pointer (types/return-type ctx "void (*)(int)")))
    (is (= {:args [:pointer :pointer] :ret :bool}
           (types/callback-signature ctx (get-in ctx [:typedefs "FX_Callback" :type]))))))
