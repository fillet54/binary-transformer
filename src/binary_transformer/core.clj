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
(s/def ::rule (s/cat :name keyword?
                     :count (s/? int?)
                     :size-path (s/? (s/coll-of keyword?))
                     :type (s/or :primitive primitive-types
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
    (get-in m [:spec-entry :count])     :const-array
    (get-in m [:spec-entry :size-path]) :var-array
    :default                            (:rule-type m)))

(defmulti decode-rule rule-type)
(defmethod decode-rule :primitive [m]
  (let [{:keys [byte-buf type]} m]
    (decode-primitive type byte-buf)))
(defmethod decode-rule :group [m]
  (let [{:keys [type]} m]
    (get (reduce decode m type) :res)))
(defmethod decode-rule :const-array [m]
  (let [{:keys [spec-entry]} m
        count (:count spec-entry)
        no-count-rule (dissoc spec-entry :count)
        no-count-m (assoc m :spec-entry no-count-rule)]
    (map (fn [_] (decode-rule no-count-m)) (range count))))
(defmethod decode-rule :var-array [m]
  (let [{:keys [res spec-entry]} m
        size-path (:size-path spec-entry)
        count (get-in res size-path)
        const-array (-> spec-entry
                        (assoc :count count)
                        (dissoc :size-path))]
    (decode-rule (assoc m :spec-entry const-array))))

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
(defmethod encode-primitive! :int32 [_ byte-buf value] (.putInt   byte-buf (unchecked-int value)))
(defmethod encode-primitive! :int16 [_ byte-buf value] (.putShort byte-buf (unchecked-short value)))
(defmethod encode-primitive! :int8  [_ byte-buf value] (.put      byte-buf (unchecked-byte value)))

(defmulti encode-rule rule-type)
(defmethod encode-rule :primitive [m]
  (let [{:keys [path byte-buf data type spec-entry]} m
        index (:index spec-entry)
        value (get-in data path)
        value (if index (nth value index) value)]
    (encode-primitive! type byte-buf value)
    m))
(defmethod encode-rule :group [m]
  (let [{:keys [spec-entry]} m]
    (-> m
        (encode (:type spec-entry)))))
(defmethod encode-rule :const-array [m]
  (let [{:keys [spec-entry]} m
        count (:count spec-entry)
        no-count-rule (dissoc spec-entry :count)
        no-count-m (assoc m :spec-entry no-count-rule)]
  (dotimes [i count]
    (encode-rule (assoc-in no-count-m [:spec-entry :index] i))))
  m)
(defmethod encode-rule :var-array [m]
  (let [{:keys [path data]} m]
    (-> m
        (assoc-in [:spec-entry :count] (count (get-in data path)))
        (update :spec-entry (fn [spec-entry] (dissoc spec-entry :size-path)))
        encode-rule))
  m)

(defmulti encode-composites :spec-entry-type)
(defmethod encode-composites :rule [m]
  (let [{:keys [path spec-entry]} m]
  (-> m
      (assoc :rule-type (first (get-in m [:spec-entry :type]))
             :type (second (get-in m [:spec-entry :type]))
             :path (conj path (:name spec-entry)))
      encode-rule
      (assoc :path path)
      (dissoc :rule-type :type))))
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
  (update m :size + (get primitive-size (:type m))))
(defmethod encode-rule-size :group [m]
  (-> m
      (assoc :spec-entry-type (:rule-type m)
             :spec-entry      (:type m))
      encode-composites-size
      (dissoc :spec-entry-type :spec-entry)))
(defmethod encode-rule-size :const-array [m]
  (let [{:keys [spec-entry]} m
        count (:count spec-entry)
        no-count-rule (dissoc spec-entry :count)
        no-count-m (assoc m :spec-entry no-count-rule)]
    (reduce (fn [m _] (encode-rule-size m)) no-count-m (range count))))
(defmethod encode-rule-size :var-array [m]
  (let [{:keys [path data spec-entry]} m
        size-path (:size-path spec-entry)
        count (count (get-in data path))]
    (-> m
        (update :size + count)
        (update-in (concat [:data] size-path) (fn [_] count))
        )))

(defmulti encode-composites-size :spec-entry-type)
(defmethod encode-composites-size :rule [m]
  (let [{:keys [path spec-entry]} m]
    (-> m
        (assoc :rule-type (first (get-in m [:spec-entry :type]))
               :type (second (get-in m [:spec-entry :type]))
               :path (conj path (:name spec-entry)))
        encode-rule-size
        (assoc :path path)
        (dissoc :rule-type :type))))
(defmethod encode-composites-size :group [m]
  (reduce encode-size-inner m (:spec-entry m)))

(defn encode-size-inner [m spec-pair]
  (-> m
      (assoc :spec-entry-type (first spec-pair)
             :spec-entry (second spec-pair))
      encode-composites-size
      (dissoc :spec-entry-type :spec-entry)))

(defn encode-normalize-and-size[spec data]
  (let [m (assoc {} :size 0 :data data :path [])]
    (reduce encode-size-inner m spec)))

(defn clj->binary
  [spec data]
  {:pre [(s/valid? ::binary-spec spec)]}
  (let [spec (s/conform ::binary-spec spec)
        {:keys [size data]} (encode-normalize-and-size spec data)
        byte-buf (ByteBuffer/allocate size)]
    (-> (reduce encode {:byte-buf byte-buf :data data :path []} spec)
        (get :byte-buf)
        .array)))
