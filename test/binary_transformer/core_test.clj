(ns binary-transformer.core-test
  (:require [clojure.test :refer :all]
            [binary-transformer.core :refer :all]))

(deftest primitive-tests
  (testing "Single integer"
    (is (= {:field1 (unchecked-int 0xABCD1234)} (transform [[:field1 :int32]] [0xAB 0xCD 0x12 0x34]))))
  (testing "Two integers"
    (is (= {:field1 (unchecked-int 0xABCD1234)
            :field2 (unchecked-int 0x34523454)}
           (transform [[:field1 :int32]
                       [:field2 :int32]]
                      [0xAB 0xCD 0x12 0x34, 0x34 0x52 0x34 0x54]))))
  (testing "Multiple primitive types"
    (is (= {:field1 (unchecked-int 0xABCD1234)
            :field2 (unchecked-byte 0xDC)
            :field3 (unchecked-short 0x3452)}
           (transform [[:field1 :int32]
                       [:field2 :int8]
                       [:field3 :int16]]
                      [0xAB 0xCD 0x12 0x34, 0xDC, 0x34 0x52 ])))))
