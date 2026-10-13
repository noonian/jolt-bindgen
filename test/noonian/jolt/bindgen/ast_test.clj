(ns noonian.jolt.bindgen.ast-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [noonian.jolt.bindgen.ast :as ast]
            [noonian.jolt.bindgen.fixture :as fx]))

(deftest signature-return
  (is (= "int" (ast/signature-return "int (int, char *)")))
  (is (= "void" (ast/signature-return "void (void)")))
  (is (= "void" (ast/signature-return "void ()")))
  (is (= "const char *" (ast/signature-return "const char *(const char *)")))
  (is (= "FX_Point" (ast/signature-return "FX_Point (void)")))
  (testing "a function returning a function pointer keeps the pointer's own parameter list"
    (is (= "void (*)(int)" (ast/signature-return "void (*(int))(int)")))
    (is (= "int (*)[3]" (ast/signature-return "int (*(void))[3]"))))
  (testing "attributes after the parameter list are not mistaken for it"
    (is (= "void" (ast/signature-return "void (int) __attribute__((noreturn))")))))

(deftest extract-fixture
  (let [{:keys [typedefs system-typedefs records enums anonymous-enums functions variables]} @fx/ir
        fns (into {} (map (juxt :name identity)) functions)]
    (testing "only declarations from the fixture header"
      (is (every? #(str/ends-with? (:file %) "fixture.h") functions))
      (is (every? #(str/ends-with? (:file %) "fixture.h") (vals records)))
      (is (not (contains? typedefs "time_t"))))
    (testing "typedefs from other headers are kept apart, with their desugared spelling"
      (is (contains? #{"long" "long long"} (get-in system-typedefs ["time_t" :desugared])))
      (is (contains? system-typedefs "va_list")))
    (testing "enums, including an anonymous one named by its typedef"
      (is (= [["FX_MODE_A" 0] ["FX_MODE_B" 5] ["FX_MODE_C" 6]] (get-in enums ["FX_Mode" :constants])))
      (is (= [["FX_ANON_X" 1] ["FX_ANON_Y" 2]] (get-in enums ["FX_Anon" :constants])))
      (testing "and one nothing names, kept for its constants"
        (is (= [[["FX_LOOSE_A" 3] ["FX_LOOSE_B" 4] ["FX_LOOSE_U" 16] ["FX_LOOSE_N" 17]]]
               (map :constants anonymous-enums)))))
    (testing "records"
      (is (= [:struct true] ((juxt :kind :complete?) (records "FX_Point"))))
      (is (= ["x" "y"] (map :name (:fields (records "FX_Point")))))
      (is (= :union (:kind (records "FX_Value"))))
      (is (false? (:complete? (records "FX_Opaque"))))
      (testing "an anonymous struct takes the name of its typedef"
        (is (= [:struct true ["x" "y"]]
               ((juxt :kind :complete? #(map :name (:fields %))) (records "FX_Vec2"))))
        (is (:typedef-named? (records "FX_Vec2")))
        (is (not (:typedef-named? (records "FX_Point")))))
      (is (true? (-> (records "FX_Bits") :fields first :bitfield?)))
      (is (false? (-> (records "FX_Point") :fields first :bitfield?)))
      (testing "an unnamed union with a field name is inlined as a nested record"
        (is (= "u" (-> (records "FX_Named") :fields second :name)))
        (is (= :union (-> (records "FX_Named") :fields second :nested :kind)))
        (is (= ["i" "f"] (map :name (-> (records "FX_Named") :fields second :nested :fields)))))
      (testing "an anonymous member is a field without a name"
        (is (nil? (-> (records "FX_Anonymous") :fields second :name)))))
    (testing "functions"
      (is (= "int" (:ret (fns "fx_add"))))
      (is (= ["a" "b"] (map :name (:params (fns "fx_add")))))
      (is (= ["int" "int"] (map :type (:params (fns "fx_add")))))
      (is (true? (:variadic? (fns "fx_log"))))
      (is (true? (:static? (fns "fx_twice"))))
      (is (false? (:static? (fns "fx_add"))))
      (is (false? (:variadic? (fns "fx_vlog"))))
      (is (= "va_list" (-> (fns "fx_vlog") :params second :type)))
      (is (= "time_t" (:ret (fns "fx_mtime"))))
      (is (= "void (*)(int)" (:ret (fns "fx_handler"))))
      (is (= "const char *const" (-> (fns "fx_const_name") :params first :type))))
    (testing "global variables"
      (is (= [["fx_counter" "int"]] (map (juxt :name :type) variables))))))
