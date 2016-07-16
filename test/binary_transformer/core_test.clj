(ns binary-transformer.core-test
  (:require [clojure.test :refer :all]
            [binary-transformer.core :refer :all]))

;;==============================================================================
;; Helper methods

(defmulti primitive-cast (fn [type x upper-range cast-fn]
                           (cond
                             (number? x) :number
                             (coll? x)   :coll)))
(defmethod primitive-cast :number [type x upper-range cast-fn]
    (if (<= 0 x upper-range)
      (cast-fn x)
      (assert false (str x " not in range of " (name type)))))
(defmethod primitive-cast :coll [type coll upper-range cast-fn]
  (map (fn [i] (primitive-cast type i upper-range cast-fn)) coll))

(defn int8   [x] (primitive-cast :int8   x 0xFF unchecked-byte))
(defn uint8  [x] (primitive-cast :uint8  x 0xFF unchecked-short))
(defn int16  [x] (primitive-cast :int16  x 0xFFFF unchecked-short))
(defn uint16 [x] (primitive-cast :uint16 x 0xFFFF unchecked-int))
(defn int32  [x] (primitive-cast :int32  x 0xFFFFFFFF unchecked-int))
(defn uint32 [x] (primitive-cast :uint32 x 0xFFFFFFFF unchecked-long))

;;==============================================================================
;; Tests

(deftest decode-simple-tests
  (testing "Single integer"
    (is (= {:field1 (int32 0xABCD1234)} (binary->clj [[:field1 :int32]] [0xAB 0xCD 0x12 0x34]))))
  (testing "Two integers"
    (is (= {:field1 (int32 0xABCD1234)
            :field2 (int32 0x34523454)}
           (binary->clj [[:field1 :int32]
                       [:field2 :int32]]
                      [0xAB 0xCD 0x12 0x34, 0x34 0x52 0x34 0x54]))))
  (testing "Multiple primitive types"
    (is (= {:field1 (int32 0xABCD1234)
            :field2 (int8 0xDC)
            :field3 (int16 0x3452)}
           (binary->clj [[:field1 :int32]
                       [:field2 :int8]
                       [:field3 :int16]]
                      [0xAB 0xCD 0x12 0x34, 0xDC, 0x34 0x52 ])))))

(deftest decode-composite-tests
  (let [header [[:field1 :int32] [:field2 :int16]]
        header-res {:field1 (int32 0xABCD1234) :field2 (int16 0x7890)}
        header-data [0xAB 0xCD 0x12 0x34, 0x78 0x90]]
    (testing "Single composition"
      (is (= header-res (binary->clj [header] header-data))))
    (testing "Inline composition"
      (is (= (merge header-res {:field3 (int32 0x98765432)})
             (binary->clj [header [:field3 :int32]] (concat header-data [0x98 0x76 0x54 0x32])))))
    (testing "Named group"
      (is (= {:header header-res :field3 (int32 0x98765432)}
             (binary->clj [[:header header] [:field3 :int32]] (concat header-data [0x98 0x76 0x54 0x32])))))
    (testing "Nested group"
      (is (= {:parent {:child {:grandchild (int8 0x98)}}}
             (binary->clj [[:parent [[:child [[:grandchild :int8]]]]]] [0x98]))))))

(deftest decode-array-tests
  (let [header [[:size :int8]]]
  (testing "Constant Array"
    (is (= {:field1 (int8 [0xAB 0x45])}
           (binary->clj [[:field1 2 :int8]] [0xAB 0x45]))))
  (testing "Variable Array"
    (is (= {:header {:size (int8 0x02)}
            :field2 (int8 [0x45 0x67])}
           (binary->clj [[:header header] [:field2 [:header :size] :int8]] [0x02 0x45 0x67]))))))

(deftest encode-simple-tests
  (testing "Single primitives"
    (is (= (int8 [0xAB 0xCD 0x12 0x34])
           (vec (clj->binary [[:field1 :int32]] {:field1 (int32 0xABCD1234)}))))))


(deftest encode-composites-tests
  (let [header [[:field1 :int32] [:field2 :int16]]
        header-data {:field1 (int32 0xABCD1234) :field2 (int32 0x7890)}
        header-res (vec (clj->bytes-array [0xAB 0xCD 0x12 0x34, 0x78 0x90]))]
    (testing "Single composition"
      (is (= header-res
             (vec (clj->binary [header] header-data)))))
    (testing "Inline composition"
      (is (= (concat header-res (int8 [0x54 0x34]))
             (vec (clj->binary [header [:field3 :int16]] (merge header-data {:field3 (int16 0x5434)}))))))
    (testing "Named group"
      (is (= (concat header-res (int8 [0x54 0x34]))
             (vec (clj->binary [[:header header] [:field3 :int16]] {:header header-data :field3 (int16 0x5434)})))))))

(deftest encode-array-tests
  (let [header [[:size :int8]]]
    (testing "Constant array"
      (is (= (int8 [0xAB 0x12 0x56])
             (vec (clj->binary [[:field1 3 :int8]] {:field1 (int8 [0xAB 0x12 0x56])})))))
    (testing "Variable array"
      (is (= (int8 [0x02 0x45 0x67])
             (vec (clj->binary [[:header header] [:field2 [:header :size] :int8]] {:field2 (int8 [0x45 0x67])})))))))

(deftest sandbox
  (let [header [[:id :int32]
               [:type :int16]
               [:other-id :int32]
               [:size :int32]]
        item [[:a :int32]]
        message [[:header header]
                [:items [:header :size] :int32]]]
  (testing "Random"
    (is (= []
           (vec
             (clj->binary message {:header {:id 0x12345678
                                            :type 0x1ABC
                                            :other-id 0x1987
                                            :size 5}
                                   :items [0x12345678 0x13345678 0x14345678 0x15345678 0x16345678]})))))))