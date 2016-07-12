(ns binary-transformer.core-test
  (:require [clojure.test :refer :all]
            [binary-transformer.core :refer :all]))

(deftest simple-tests
  (testing "Single integer"
    (is (= {:field1 (unchecked-int 0xABCD1234)} (binary->clj [[:field1 :int32]] [0xAB 0xCD 0x12 0x34]))))
  (testing "Two integers"
    (is (= {:field1 (unchecked-int 0xABCD1234)
            :field2 (unchecked-int 0x34523454)}
           (binary->clj [[:field1 :int32]
                       [:field2 :int32]]
                      [0xAB 0xCD 0x12 0x34, 0x34 0x52 0x34 0x54]))))
  (testing "Multiple primitive types"
    (is (= {:field1 (unchecked-int 0xABCD1234)
            :field2 (unchecked-byte 0xDC)
            :field3 (unchecked-short 0x3452)}
           (binary->clj [[:field1 :int32]
                       [:field2 :int8]
                       [:field3 :int16]]
                      [0xAB 0xCD 0x12 0x34, 0xDC, 0x34 0x52 ])))))

(deftest composite-tests
  (let [header [[:field1 :int32] [:field2 :int16]]
        header-res {:field1 (unchecked-int 0xABCD1234) :field2 (unchecked-short 0x7890)}
        header-data [0xAB 0xCD 0x12 0x34, 0x78 0x90]]
    (testing "Single composition"
      (is (= header-res (binary->clj [header] header-data))))
    (testing "Inline composition"
      (is (= (merge header-res {:field3 (unchecked-int 0x98765432)})
             (binary->clj [header [:field3 :int32]] (concat header-data [0x98 0x76 0x54 0x32])))))
    (testing "Named group"
      (is (= {:header header-res :field3 (unchecked-int 0x98765432)}
             (binary->clj [[:header header] [:field3 :int32]] (concat header-data [0x98 0x76 0x54 0x32])))))))

(deftest encode-simple-tests
  (testing "Single primitives"
    (is (= (vec (clj->bytes-array [0xAB 0xCD 0x12 0x34]))
           (vec (clj->binary [[:field1 :int32]] {:field1 (unchecked-int 0xABCD1234)}))))))


(deftest encode-composites-tests
  (let [header [[:field1 :int32] [:field2 :int16]]
        header-data {:field1 (unchecked-int 0xABCD1234) :field2 (unchecked-short 0x7890)}
        header-res (vec (clj->bytes-array [0xAB 0xCD 0x12 0x34, 0x78 0x90]))]
    (testing "Single composition"
      (is (= header-res
             (vec (clj->binary [header] header-data)))))
    (testing "Inline composition"
      (is (= (concat header-res (vec (clj->bytes-array [0x54 0x34])))
             (vec (clj->binary [header [:field3 :int16]] (merge header-data {:field3 (unchecked-short 0x5434)}))))))
    (testing "Named group"
      (is (= (concat header-res (vec (clj->bytes-array [0x54 0x34])))
             (vec (clj->binary [[:header header] [:field3 :int16]] {:header header-data :field3 (unchecked-short 0x5434)})))))))
