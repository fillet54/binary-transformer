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
(s/def ::rule (s/cat :name keyword? :count (s/? int?) :type primitive-types))
(s/def ::binary-spec (s/coll-of ::rule))

;==============================================================================
; Conversion functions

(defn ^:private ->bytes-array [bytes]
  (let [bytes (->> bytes
                   (s/conform ::bytes)
                   (apply hash-map))]
  (or (:byte-array bytes)
      (into-array Byte/TYPE (map unchecked-byte (:num-sequence bytes))))))

;==============================================================================
; Transformation functions

(defn extract-simple [f m rule]
  (assoc-in m [:res (:name rule)] (f (:data m))))

(defmulti transform-rule #(:type %2))
(defmethod transform-rule :int32 [m rule] (extract-simple #(.getInt %) m rule))
(defmethod transform-rule :int16 [m rule] (extract-simple #(.getShort %) m rule))
(defmethod transform-rule :int8 [m rule] (extract-simple #(.get %) m rule))

(defn transform
  [spec bytes]
  {:pre [(s/valid? ::binary-spec spec)
         (s/valid? ::bytes bytes)]}
  (let [byte-buf (ByteBuffer/wrap (->bytes-array bytes))
        spec (s/conform ::binary-spec spec)]
    (get (reduce transform-rule {:res {} :data byte-buf} spec) :res)))
