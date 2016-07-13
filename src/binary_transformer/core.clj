(ns binary-transformer.core
  (:require [clojure.spec :as s])
  (:import (java.nio ByteBuffer)))


;==============================================================================
; Spec Definitions

(s/def ::bytes (s/or :byte-array bytes?
                     :num-sequence (s/* (s/and int?
                                             #(>= % 0)
                                             #(<= % 0xFFFF)))))

(def primitive-types #{:int32 :int16 :int8})
(s/def ::rule (s/cat :name keyword? :count (s/? int?) :type (s/or :primitive primitive-types
                                                                  :group     ::binary-spec)))
(s/def ::binary-spec (s/coll-of (s/or :rule  ::rule
                                      :group ::binary-spec)))

;==============================================================================
; Conversion functions

(defn clj->bytes-array [bytes]
  (let [bytes (->> bytes
                   (s/conform ::bytes)
                   (apply hash-map))]
  (or (:byte-array bytes)
      (into-array Byte/TYPE (map unchecked-byte (:num-sequence bytes))))))

;==============================================================================
; Decode functions

(declare decode)

(defmulti  decode-primitive (fn [type _] type))
(defmethod decode-primitive :int32 [_ byte-buf] (.getInt byte-buf))
(defmethod decode-primitive :int16 [_ byte-buf] (.getShort byte-buf))
(defmethod decode-primitive :int8  [_ byte-buf] (.get byte-buf))

(defmulti decode-rule #(cond
                        (get-in % [:spec-entry :count]) :array
                        :default                        (:rule-type %)))
(defmethod decode-rule :primitive [m]
  (let [{:keys [byte-buf type]} m]
    (decode-primitive type byte-buf)))
(defmethod decode-rule :group [m]
  (let [{:keys [type]} m]
    (get (reduce decode (assoc m :res {}) type) :res)))
(defmethod decode-rule :array [m]
  (let [{:keys [spec-entry]} m
        count (:count spec-entry)
        no-count-rule (dissoc spec-entry :count)]
    (map (fn [_] (decode-rule (assoc m :spec-entry no-count-rule))) (range count))))

(defmulti decode-composites :spec-entry-type)
(defmethod decode-composites :rule [m]
  (let [{:keys [spec-entry]} m
        value (-> m
                  (assoc :rule-type (first  (:type spec-entry))
                         :type      (second (:type spec-entry)))
                  decode-rule)
        field (:name spec-entry)]
    (assoc-in m [:res field] value)))
(defmethod decode-composites :group [m]
  (reduce decode m (:spec-entry m)))

(defn decode [m spec-pair]
  (-> m
      (assoc :spec-entry-type (first spec-pair)
             :spec-entry (second spec-pair))
      decode-composites
      (dissoc :spec-entry-type :spec-entry)))

(defn binary->clj
  [spec bytes]
  {:pre [(s/valid? ::binary-spec spec)
         (s/valid? ::bytes bytes)]}
  (let [byte-buf (ByteBuffer/wrap (clj->bytes-array bytes))
        spec (s/conform ::binary-spec spec)]
    (get (reduce decode {:res {} :byte-buf byte-buf} spec) :res)))


;==============================================================================
; Encode functions

(declare encode-composites)
(declare encode-composites-size)
(declare encode-size-inner)

(defmulti  encode-primitive! (fn [type _ _ _] type))
(defmethod encode-primitive! :int32 [_ field byte-buf data] (.putInt   byte-buf (get data field)))
(defmethod encode-primitive! :int16 [_ field byte-buf data] (.putShort byte-buf (get data field)))
(defmethod encode-primitive! :int8  [_ field byte-buf data] (.put      byte-buf (get data field)))

(defmulti encode-rule #(first (:type %2)))
(defmethod encode-rule :primitive [state rule]
  (let [{:keys [byte-buf data]} state
        type (second (:type rule))
        field (:name rule)]
    (encode-primitive! type field byte-buf data)
    state))
(defmethod encode-rule :group [state rule]
  (let [data (:data state)
        scoped-data (get data (:name rule))]
    (encode-composites (assoc state :data scoped-data) (:type rule)))
  state)

(defmulti encode-composites #(first %2))
(defmethod encode-composites :rule [state spec]
  (encode-rule state (second spec)))
(defmethod encode-composites :group [state spec]
  (reduce encode-composites state (second spec)))


(def primitive-size {:int32 4 :int16 2 :int8 1})

(defmulti encode-rule-size :rule-type)
(defmethod encode-rule-size :primitive [m]
  (get primitive-size (:type m)))
(defmethod encode-rule-size :group [m]
  (-> {}
      (assoc :spec-entry-type (:rule-type m)
             :spec-entry      (:type m))
      encode-composites-size))


(defmulti encode-composites-size :spec-entry-type)
(defmethod encode-composites-size :rule [m]
  (-> {}
      (assoc :rule-type (first (get-in m [:spec-entry :type]))
             :type (second (get-in m [:spec-entry :type])))
      encode-rule-size))
(defmethod encode-composites-size :group [m]
  (reduce encode-size-inner 0 (:spec-entry m)))

(defn encode-size-inner [size spec-pair]
  (+ size
     (-> {}
         (assoc :spec-entry-type (first spec-pair)
                :spec-entry (second spec-pair))
         encode-composites-size)))

(defn encode-size [spec m]
  (reduce encode-size-inner 0 spec))


(defn clj->binary
  [spec m]
  {:pre [(s/valid? ::binary-spec spec)]}
  (let [spec (s/conform ::binary-spec spec)
        size (encode-size spec m)
        byte-buf (ByteBuffer/allocate size)]
    (.array (get (reduce encode-composites {:byte-buf byte-buf :data m} spec) :byte-buf))))

(comment

  (def header [[:header1 :int16]
               [:header2 :int16]])
  (def group1 [[group]])

  (s/conform ::binary-spec [[[[:field1 :int32]]] [:header header] [:field2 :int32]])

  (encode-size (s/conform ::binary-spec [[:field1 :int32]]) {:field1 (unchecked-int 0xABCD1234)})

  )