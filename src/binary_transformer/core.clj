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

(defn ^:private clj->bytes-array [bytes]
  (let [bytes (->> bytes
                   (s/conform ::bytes)
                   (apply hash-map))]
  (or (:byte-array bytes)
      (into-array Byte/TYPE (map unchecked-byte (:num-sequence bytes))))))

;==============================================================================
; Transformation functions

(declare transform-composites)

(defn extract-simple [f m rule]
  (assoc-in m [:res (:name rule)] (f (:data m))))

(defmulti transform-primitive #(second (:type %2)))
(defmethod transform-primitive :int32 [m rule] (extract-simple #(.getInt %) m rule))
(defmethod transform-primitive :int16 [m rule] (extract-simple #(.getShort %) m rule))
(defmethod transform-primitive :int8 [m rule] (extract-simple #(.get %) m rule))

(defmulti transform-rule #(first (:type %2)))
(defmethod transform-rule :primitive [state rule]
  (transform-primitive state rule))
(defmethod transform-rule :group [state rule]
  (assoc-in state [:res (:name rule)] (get (transform-composites state (:type rule)) :res)))

(defmulti transform-composites #(first %2))
(defmethod transform-composites :rule [state spec]
  (transform-rule state (second spec)))
(defmethod transform-composites :group [state spec]
  (reduce transform-composites state (second spec)))


(defn binary->clj
  [spec bytes]
  {:pre [(s/valid? ::binary-spec spec)
         (s/valid? ::bytes bytes)]}
  (let [byte-buf (ByteBuffer/wrap (clj->bytes-array bytes))
        spec (s/conform ::binary-spec spec)]
    (get (reduce transform-composites {:res {} :data byte-buf} spec) :res)))

(comment

  (def header [[:header1 :int16]
               [:header2 :int16]])

  (s/conform ::binary-spec [[:field1 :int32] [:header header] [:field2 :int32]])

  )