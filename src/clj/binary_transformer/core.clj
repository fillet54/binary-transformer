(ns binary-transformer.core
  (:require [clojure.spec :as s])
  (:import (java.nio ByteBuffer)
           (fiftycuatro.util WrappingBitBuffer)))

;==============================================================================
; Spec Definitions

(s/def ::bytes (s/or :byte-array bytes?
                     :num-sequence (s/* (s/and int?
                                             #(>= % 0)
                                             #(<= % 0xFFFF)))))

;; Note that args are conformed later in the process
(s/def ::entry (s/cat :name (some-fn keyword? int?)
                      :type keyword?
                      :raw-args (s/* ::s/any)))

(s/def ::binary-spec (s/* (s/or :entry        ::entry
                                :nested-spec  ::binary-spec)))

;; Extend to conform arguments
(defmulti entry-args-spec :type)
(defmethod entry-args-spec :group [_] ::binary-spec)
(s/def ::size (s/or :const-size int? :path-to-size (s/coll-of keyword?)))
(s/def ::type (s/or :spec (s/spec ::binary-spec)
                    :inline keyword?))
(defmethod entry-args-spec :array [_] (s/keys* :req-un [::size ::type]))

(s/def ::n-bits int?)
(defmethod entry-args-spec :int [_] (s/keys* :req-un [::n-bits]))
(defmethod entry-args-spec :uint [_] (s/keys* :req-un [::n-bits]))

(defmethod entry-args-spec :default [_] (s/* ::s/any))
;==============================================================================
; Conversion functions

(defn clj->bytes-array [bytes]
  (let [bytes (->> bytes
                   (s/conform ::bytes)
                   (apply hash-map))]
    (or (:byte-array bytes)
        (byte-array (count (:num-sequence bytes)) (:num-sequence bytes)))))

;==============================================================================
; Encode/Decode functions

(defn reduce-spec [m spec f]
  (let [m (if (:spec m) m (assoc m :spec spec))]
    (reduce (fn [m spec-entry]
              (let [spec-entry (apply hash-map spec-entry)]
                (if (:entry spec-entry)
                  (let [entry (:entry spec-entry)
                        args (s/conform (entry-args-spec entry) (:raw-args entry))
                        entry (assoc entry :args args)]
                    (f (assoc m :entry entry)))
                  (reduce-spec m (:nested-spec spec-entry) f))))
            m spec)))


(defn type-selector [m] (get-in m [:entry :type]))

;(defmulti encode-preprocess type-selector)
;(defmulti encode-type type-selector)
;(defmulti decode-preprocess type-selector)
(defmulti decode-type type-selector)
(defmethod decode-type :int [m]
  (let [{:keys [entry path bits]} m
        field-path (conj path (:name entry))
        value (.getInt bits (get-in entry [:args :n-bits]))]
    (assoc-in m field-path value)))
(defmethod decode-type :uint [m]
  (let [{:keys [entry path bits]} m
        field-path (conj path (:name entry))
        value (.getUnsignedInt bits (get-in entry [:args :n-bits]))]
    (assoc-in m field-path value)))

(defmethod decode-type :group [m]
  (let [{:keys [entry path]} m
        group-path (conj path (:name entry))
        group-spec (:args entry)]
    (-> m
        (assoc :path group-path :entry group-spec)
        (reduce-spec group-spec decode-type)
        (assoc :path path :entry entry))))

(defmethod decode-type :array [m]
  (let [{:keys [entry path]} m
        args (:args entry)
        size-opts (apply hash-map (:size args))
        size (or (:const-size size-opts)
                 (get-in m (concat [:result] (:path-to-size size-opts))))
        spec (->> (range size)
                  (map (fn [i]
                         (let [type (apply hash-map (:type args))]
                           (cond
                             (:spec type) [i :group (s/unform ::binary-spec (:spec type))]
                             (:inline type) (into [i (:inline type)] (:type-options args))))))
                  (s/conform ::binary-spec))
        array-path (conj path (:name entry))]
    (-> m
        (assoc :path array-path)
        (assoc-in array-path [])
        (reduce-spec spec decode-type)
        (assoc :path path))))

(defn decode [spec byte-buf]
  (let [spec (s/conform ::binary-spec spec)
        bits (WrappingBitBuffer/wrap (clj->bytes-array byte-buf))]
    (-> {:path [:result] :result {} :bits bits}
        ;(reduce-spec spec decode-preprocess)
        (reduce-spec spec decode-type)
        :result)))

