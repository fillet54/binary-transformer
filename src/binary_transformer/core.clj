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

(defn rule-type [m]
  (cond
    (get-in m [:spec-entry :count]) :const-array
    :default                        (:rule-type m)))

(defmulti decode-rule rule-type)
(defmethod decode-rule :primitive [m]
  (let [{:keys [byte-buf type]} m]
    (decode-primitive type byte-buf)))
(defmethod decode-rule :group [m]
  (let [{:keys [type]} m]
    (get (reduce decode (assoc m :res {}) type) :res)))
(defmethod decode-rule :const-array [m]
  (let [{:keys [spec-entry]} m
        count (:count spec-entry)
        no-count-rule (dissoc spec-entry :count)
        no-count-m (assoc m :spec-entry no-count-rule)]
    (map (fn [_] (decode-rule no-count-m)) (range count))))

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

(declare encode)
(declare encode-composites)
(declare encode-composites-size)
(declare encode-size-inner)

(defmulti  encode-primitive! (fn [type _ _] type))
(defmethod encode-primitive! :int32 [_ byte-buf value] (.putInt   byte-buf value))
(defmethod encode-primitive! :int16 [_ byte-buf value] (.putShort byte-buf value))
(defmethod encode-primitive! :int8  [_ byte-buf value] (.put      byte-buf value))

(defmulti encode-rule rule-type)
(defmethod encode-rule :primitive [m]
  (let [{:keys [byte-buf data type spec-entry]} m
        field (:name spec-entry)
        index (:index spec-entry)
        value (get data field)
        value (if index (nth value index) value)]
    (encode-primitive! type byte-buf value)
    m))
(defmethod encode-rule :group [m]
  (let [{:keys [data spec-entry]} m
        field (:name spec-entry)
        scoped-data (get data field)]
    (encode (assoc m :data scoped-data) (:type spec-entry)))
  m)
(defmethod encode-rule :const-array [m]
  (let [{:keys [spec-entry]} m
        count (:count spec-entry)
        no-count-rule (dissoc spec-entry :count)
        no-count-m (assoc m :spec-entry no-count-rule)]
  (dotimes [i count]
    (encode-rule (assoc-in no-count-m [:spec-entry :index] i))))
  m)

(defmulti encode-composites :spec-entry-type)
(defmethod encode-composites :rule [m]
  (-> m
      (assoc :rule-type (first (get-in m [:spec-entry :type]))
             :type (second (get-in m [:spec-entry :type])))
      encode-rule))
(defmethod encode-composites :group [m]
  (reduce encode m (:spec-entry m)))

(defn encode [m spec-pair]
  (-> m
      (assoc :spec-entry-type (first spec-pair)
             :spec-entry      (second spec-pair))
      encode-composites))


;;==============================================================================
;; Spec size calculation

(def primitive-size {:int32 4 :int16 2 :int8 1})

(defmulti encode-rule-size rule-type)
(defmethod encode-rule-size :primitive [m]
  (get primitive-size (:type m)))
(defmethod encode-rule-size :group [m]
  (-> {}
      (assoc :spec-entry-type (:rule-type m)
             :spec-entry      (:type m))
      encode-composites-size))
(defmethod encode-rule-size :const-array [m]
  (let [{:keys [spec-entry]} m
        count (:count spec-entry)
        no-count-rule (dissoc spec-entry :count)
        no-count-m (assoc m :spec-entry no-count-rule)]
    (* count (encode-rule-size no-count-m))))

(defmulti encode-composites-size :spec-entry-type)
(defmethod encode-composites-size :rule [m]
  (-> m
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
    (-> (reduce encode {:byte-buf byte-buf :data m} spec)
        (get :byte-buf)
        .array)))

(comment
  (def header [[:header1 :int16]
               [:header2 :int16]])
  (def group1 [[group]])

  (s/conform ::binary-spec [[[[:field1 :int32]]] [:header header] [:field2 :int32]])
  (encode-size
    (s/conform ::binary-spec [[:group [[:field1 5 :int32]]]])
    {:field1 (unchecked-int 0xABCD1234)})

  (s/conform ::binary-spec [[:group 5 [[:field1 :int32]]]])

  (def header [[:sync :uint32 0xCFCFCFCF]
               [:num-payloads :unint32]])

  (def payload [[:id :uint16]
                [:data :uint16]])

  (def my-message [[:header header]
                   [:payload-items [:header :num-payloads] payload]
                   [:option [:header :type] {0 type1 2 type2}]])





  )