(ns binary-transformer.core
  (:require [clojure.spec :as s])
  (:import (java.nio ByteBuffer)))

;==============================================================================
; Spec Definitions

(s/def ::bytes (s/or :byte-array bytes?
                     :num-sequence (s/* (s/and int?
                                             #(>= % 0)
                                             #(<= % 0xFFFF)))))


(defn type-selector [m] (get-in m [:entry :type]))

;; Extend to conform arguments
(defmulti entry-args-spec :type)
(defmethod entry-args-spec :group [_] ::binary-spec)
(s/def ::size (s/or :const-size int? :path-to-size (s/coll-of keyword?)))
(defmethod entry-args-spec :array [_] (s/cat :spec ::binary-spec
                                             :options (s/keys* :req-un [::size])))
(defmethod entry-args-spec :default [_] (s/* ::s/any))

;; Note that args are conformed later in the process
(s/def ::entry (s/cat :name keyword?
                      :type keyword?
                      :args (s/* ::s/any)))

(s/def ::binary-spec (s/coll-of (s/or :entry        ::entry
                                      :nested-spec  ::binary-spec)))

;==============================================================================
; Conversion functions

(defn clj->bytes-array [bytes]
  (let [bytes (->> bytes
                   (s/conform ::bytes)
                   (apply hash-map))]
    (or (:byte-array bytes)
        (into-array Byte/TYPE (map unchecked-byte (:num-sequence bytes))))))


;==============================================================================
; Encode/Decode functions

(defn reduce-spec [m spec f]
  (println spec)
  (let [m (if (:spec m) m (assoc m :spec spec))]
    (reduce (fn [m spec-entry]
              (let [spec-entry (apply hash-map spec-entry)]
                (if (:entry spec-entry)
                  (let [entry (:entry spec-entry)
                        args (s/conform (entry-args-spec entry) (:args entry))
                        entry (assoc entry :args args)]
                    (f (assoc m :entry entry)))
                  (reduce-spec m (:nested-spec spec-entry) f))))
            m spec)))

;(defmulti encode-preprocess type-selector)
;(defmulti encode-type type-selector)
;(defmulti decode-preprocess type-selector)
(defmulti decode-type type-selector)
(defmethod decode-type :int32 [m]
  (let [{:keys [entry path bits]} m
        path (conj path (:name entry))
        value (.getInt bits)]
    (assoc-in m path value)))

(defmethod decode-type :int16 [m]
  (let [{:keys [entry path bits]} m
        path (conj path (:name entry))
        value (.getShort bits)]
    (assoc-in m path value)))

(defmethod decode-type :group [m]
  (let [{:keys [entry path]} m
        group-path (conj path (:name entry))
        group-spec (:args entry)]
    (-> m
        (assoc :path group-path :entry group-spec)
        (reduce-spec group-spec decode-type)
        (assoc :path path :entry entry))))

(defmethod decode-type :array-item [m]
  (let [args (get-in m [:entry :args])
        {:keys [const-size path-to-size]}  (apply hash-map (get-in args [:options :size]))
        size (or const-size
                 (get-in m (concat [:result] path-to-size)))
        spec (repeat size (:spec args))]))

(defmethod decode-type :array [m]
  (let [args (get-in m [:entry :args])
        {:keys [const-size path-to-size]}  (apply hash-map (get-in args [:options :size]))
        size (or const-size
                 (get-in m (concat [:result] path-to-size)))
        spec (repeat size (:spec args))]
    (reduce-spec m spec decode-type)))

(defn decode [spec byte-buf]
  (let [spec (s/conform ::binary-spec spec)
        bits (ByteBuffer/wrap (clj->bytes-array byte-buf))]
    (-> {:path [:result] :result {} :bits bits}
        ;(reduce-spec spec decode-preprocess)
        (reduce-spec spec decode-type)
        :result)))


(comment


  (def header [[:id       :int32]
               [:type     :group [[:inner1 :int16]
                                  [:inner2 :int16]]]
               [:otherttd :int32]
               [:size     :int32]])

  (def item [[:a :int16]])

  (def message [[:header :group header]
                [:items  :array item :size [:header :size]]])


 (decode message [0x12 0x34 0x56 0x78, 0x00 0x00, 0x00 0x20, 0x34 0x56 0x78 0x90, 0x00 0x00 0x00 0x03, 0x00 0x01, 0x00 0x10, 0x00 0x20])

  (s/conform ::binary-spec message )
  (s/explain ::binary-spec [[:item :group item]])


  (def array-args [[[:a :int32]] :size [:header :size]])
  (s/conform (entry-args-spec {:type :array}) array-args)
  (s/explain (entry-args-spec {:type :array}) array-args)
  ;[:id :type :ref? :option-hash]

  ;(def my-choices {1 choice1
  ;                 2 choice2})

  ;(def reasons {1 reason1
  ;              2 reason2})

  ;[[:sync   :int32     :value 0x34 :nbits 2]
  ; [:header :group     header   :size 1]
  ; [:items  :array     item     :size [:header :size]]
  ; [:body   :choice-of commands :using [:header :type]]
  ; [:reason :enum32    reasons]
  ; [:number :scalar    :scale 1.4565 :offset 2]]



  ;encode-preprocess :type => {:size :data :path :entry :spec}
  ;decode-preprocess :type => {:data :path :entry :spec}

  ;encode :type => {:bytes :data :path :entry :spec}
  ;decode :type => {:bits :result :path :entry :spec}

  ;(def message [[:header :group spe]
  ;              []])

  ;(vec
  ;  (clj->binary message {:header {:id 0x12345678
  ;                                 :type 0x1ABC
  ;                                 :other-id 0x1987
  ;                                 :size 5}
  ;                        :items [0x12345678 0x13345678 0x14345678 0x15345678 0x16345678]}))





  (def items [{:a 0x12345678 :b 0x1987}
              {:a 0x13345678 :b 0x1887}
              {:a 0x14345678 :b 0x1787}
              {:a 0x15345678 :b 0x1687}
              {:a 0x16345678 :b 0x1587}])



  )