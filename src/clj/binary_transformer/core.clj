(ns binary-transformer.core
  (:require [clojure.spec :as s]
            [clojure.core.match :as m])
  (:import (fiftycuatro.util WrappingBitBuffer))
  (:refer-clojure :exclude [compile]))

;==============================================================================
; Spec Definitions

(s/def ::bytes (s/or :byte-array bytes?
                     :num-sequence (s/* (s/and int?
                                               #(>= % 0)
                                               #(<= % 0xFFFF)))))

;; Note that args are conformed later in the process
(s/def ::entry (s/cat :name (some-fn keyword? int? nil?)
                      :type keyword?
                      :args (s/* any?)))

(s/def ::binary-spec (s/* (s/or :entry  ::entry
                                :nested ::binary-spec)))

;; Extend to conform arguments
(defmulti entry-args-spec :type)

;; group
(defmethod entry-args-spec ::group [_] ::binary-spec)

;; array
(s/def ::size (s/or :const-size int?
                    :path-to-size (s/coll-of keyword?)))
(s/def ::type (s/or :spec (s/spec ::binary-spec)
                    :inline keyword?))
(s/def ::type-args (s/* any?))
(defmethod entry-args-spec ::array [_] (s/keys* :req [::size ::type]
                                                :opt [::type-args]))

;; integers
(s/def ::n-bits int?)
(defmethod entry-args-spec ::int [_] (s/keys* :req [::n-bits]))
(defmethod entry-args-spec ::uint [_] (s/keys* :req [::n-bits]))

(defmethod entry-args-spec :default [_] (s/* any?))

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

(declare compile-entry)

;; Compile fns take a env and an entry and return a decode fn
;; Decode fns take a result map and bitbuffer and returns a result map
(defmulti compile :type)

;==============================================================================
; Integer types

(defmethod compile ::int8   [entry] (compile (assoc entry :args {::n-bits 8}  :type ::int)))
(defmethod compile ::int16  [entry] (compile (assoc entry :args {::n-bits 16} :type ::int)))
(defmethod compile ::int32  [entry] (compile (assoc entry :args {::n-bits 32} :type ::int)))
(defmethod compile ::int64  [entry] (compile (assoc entry :args {::n-bits 64} :type ::int)))
(defmethod compile ::uint8  [entry] (compile (assoc entry :args {::n-bits 8}  :type ::uint)))
(defmethod compile ::uint16 [entry] (compile (assoc entry :args {::n-bits 16} :type ::uint)))
(defmethod compile ::uint32 [entry] (compile (assoc entry :args {::n-bits 32} :type ::uint)))
(defmethod compile ::uint64 [entry] (compile (assoc entry :args {::n-bits 64} :type ::uint)))

(defmethod compile ::int [entry]
  (let [name (:name entry)
        n-bits (get-in entry [:args ::n-bits])]
    (fn [m path bits]
      (let [path (if name (conj path name) path)]
        (assoc-in m path (.getInt bits n-bits))))))

(defmethod compile ::uint [entry]
  (let [name (:name entry)
        n-bits (get-in entry [:args ::n-bits])]
    (fn [m path bits]
      (let [path (if name (conj path name) path)]
        (assoc-in m path (.getUnsignedInt bits n-bits))))))

;==============================================================================
; Composite types

(defmethod compile ::group [entry]
  (let [{:keys [args name]} entry
        decode-fns (reduce compile-entry [] args)]
    (fn [m path bits]
      (let [path (if name (conj path name) path)]
        (reduce
          (fn [m f] (f m path bits))
          m decode-fns)))))

(defmethod compile ::array [entry]
  (let [{:keys [args name]} entry
        resolve-size (m/match (::size args)
                              [:const-size size] (fn [_] size)
                              [:path-to-size size-path] (fn [m] (get-in m size-path)))
        decode-fns (m/match (::type args)
                            [:inline type] (let [entry-args (s/conform (entry-args-spec {:type type}) (::type-args args))
                                                 entry (-> (s/conform ::entry [nil type])
                                                           (assoc :args entry-args))]
                                             (compile entry))
                            [:spec spec] (reduce compile-entry [] spec))]
    (fn [m path bits]
      (let [path (if name (conj path name) path)
            size (resolve-size m)
            m (assoc-in m path [])]
        (if (fn? decode-fns)
          (reduce (fn [m index] (decode-fns m (conj path index) bits)) m (range size))
          (reduce (fn [m index]
                    (let [path (conj path index)
                          m (assoc-in m path {})]
                      (reduce (fn [m f] (f m path bits)) m decode-fns))) m (range size)))))))


(defn compile-entry [fns entry]
  (m/match entry
           [:entry e] (let [args (s/conform (entry-args-spec e) (:args e))
                            entry (assoc e :args args)]
                        (conj fns (compile entry)))
           [:nested spec] (into [] (concat fns (reduce compile-entry fns spec)))))

;; Returns a sequence of decode fns
(defn compile-spec [spec]
  (let [spec (s/conform ::binary-spec spec)]
    (reduce compile-entry [] spec)))

;; Executes a sequence of decocde fns
(defn decode-spec [fns bytes]
  (let [bits (WrappingBitBuffer/wrap (clj->bytes-array bytes))]
    (reduce (fn [m f] (f m [] bits)) {} fns)))


(defn decode [spec bytes]
  (decode-spec (compile-spec spec) bytes))


