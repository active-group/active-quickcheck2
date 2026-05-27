(ns active.quickcheck2.realm
  (:require
   [active.data.realm :as realm]
   [active.data.realm.internal.records :as realm-record]
   [active.data.realm.inspection :as realm-inspection]
   [active.quickcheck2.arbitrary :as arbitrary]
   [active.quickcheck2.generator :as generator]
   [active.quickcheck2.generator-applicative :as generator-applicative])
  #?(:clj (:import
           (java.lang UnsupportedOperationException)
           (java.util UUID))))

(def ^:private arbitrary-key ::arbitrary)

(defn with-arbitrary
  [realm arbitrary]
  (realm/with-metadata realm arbitrary-key arbitrary))

(defn- unsupported-exception [^String message]
  #?(:clj  (UnsupportedOperationException. message)
     :cljs (js/Error. message)))

(defn- unsupported-realm-execption [realm]
  (let [message (str "arbitrary not implemented for realm " realm)]
    (unsupported-exception message)))

(def ^:private arbitrary-uuid
  (arbitrary/make-arbitrary
   (generator-applicative/generator-map
    (fn [^String s]
      (let [bytes* #?(:clj  (.getBytes s)
                      :cljs (let [utf8-encode (js/TextEncoder.)]
                              (.encode utf8-encode s)))]
        (UUID/nameUUIDFromBytes bytes*)))
    (generator/choose-string generator/choose-alphanumeric-char 10))))

(defn- arbitrary-record [ctor arbitrary-fields]
  ;; Note: arbitrary/arbitrary-record doesn't use of the 'accessors' argument, yet.
  (apply arbitrary/arbitrary-record ctor nil arbitrary-fields))

(defn- arbitrary-map-with-keys [m]
  (arbitrary-record (fn [& vs]
                      (zipmap (keys m) vs))
                    (vals m)))

(defn arbitrary
  [realm]
  (or (get (realm-inspection/metadata realm) arbitrary-key)
      (condp = realm
        realm/natural arbitrary/arbitrary-natural
        realm/integer arbitrary/arbitrary-integer
        realm/rational arbitrary/arbitrary-rational
        realm/real arbitrary/arbitrary-float
        realm/char arbitrary/arbitrary-char
        realm/string arbitrary/arbitrary-string
        realm/symbol arbitrary/arbitrary-symbol
        realm/keyword arbitrary/arbitrary-keyword
        realm/boolean arbitrary/arbitrary-boolean
        realm/uuid arbitrary-uuid
        realm/any arbitrary/arbitrary-any
        ;; TODO: What other numbers would we like to produce?
        realm/number (arbitrary/arbitrary-mixed [[integer? arbitrary/arbitrary-integer]
                                                 [double? arbitrary/arbitrary-float]
                                                 #?(:clj [#(instance? java.lang.Long %) arbitrary/arbitrary-long])])

        (cond
          (realm-inspection/sequence-of? realm)
          (arbitrary/arbitrary-list (arbitrary (realm-inspection/sequence-of-realm-realm realm)))

          (realm-inspection/integer-from-to? realm)
          (arbitrary/arbitrary-integer-from-to (realm-inspection/integer-from-to-realm-from realm)
                                               (realm-inspection/integer-from-to-realm-to realm))

          (realm-inspection/optional? realm)
          (arbitrary/arbitrary-mixed [[nil? (arbitrary/arbitrary-one-of = nil)]
                                      [some? (arbitrary (realm-inspection/optional-realm-realm realm))]])
          
          (realm-inspection/union? realm)
          (arbitrary/arbitrary-mixed (mapv (fn [realm]
                                             ;; Note: arbitrary-mixed does not use the predicate yet.
                                             [(realm-inspection/predicate realm) (arbitrary realm)])
                                           (realm-inspection/union-realm-realms realm)))
          
          (realm-inspection/set-of? realm)
          (arbitrary/arbitrary-set (arbitrary (realm-inspection/set-of-realm-realm realm)))
          
          (realm-inspection/enum? realm)
          (arbitrary/make-arbitrary (generator/choose-one-of (realm-inspection/enum-realm-values realm)))

          (realm-inspection/tuple? realm)
          (apply arbitrary/arbitrary-tuple (map arbitrary (realm-inspection/tuple-realm-realms realm)))

          (realm-inspection/named? realm)
          (arbitrary (realm-inspection/named-realm-realm realm))

          (realm-inspection/delayed? realm)
          (arbitrary @(realm-inspection/delayed-realm-delay realm))

          (realm-inspection/map-of? realm)
          (arbitrary/arbitrary-map (arbitrary (realm-inspection/map-of-realm-key-realm realm))
                                   (arbitrary (realm-inspection/map-of-realm-value-realm realm)))

          (realm-inspection/map-with-keys? realm)
          (let [m (realm-inspection/map-with-keys-realm-map realm)]
            (arbitrary-map-with-keys (zipmap (keys m) (map arbitrary (vals m)))))

          (realm-inspection/map-with-tag? realm)
          (arbitrary/arbitrary-map (arbitrary/arbitrary-one-of = (realm-inspection/map-with-tag-realm-key realm))
                                   (arbitrary (realm-inspection/map-with-tag-realm-value )))

          (realm-inspection/record? realm)
          (arbitrary-record (realm-inspection/record-realm-constructor realm)
                            (map arbitrary (map realm-inspection/record-realm-field-realm (realm-inspection/record-realm-fields realm))))

          (realm-inspection/intersection? realm)
          (let [realms (realm-inspection/intersection-realm-realms realm)]
            ;; for a slight performance inprovement (but a common case via 'realm/restricted'), try to separate from-predicate realms and others:
            (let [{others false preds true} (group-by (comp boolean realm-inspection/from-predicate?) realms)]
              (if (empty? others)
                ;; only predicate realms? This will likely not be able to generate any
                ;; values (as 'any' cannot generate everything), but we have no chance but to give it a try:
                (arbitrary/such-that arbitrary/arbitrary-any
                                     (apply every-pred (concat (map realm-inspection/predicate preds))))
                ;; use all non-predicate realms 'in turn'; this might lead to slightly
                ;; better runtime behaviour than using just one as the basis:
                (arbitrary/such-that (if (empty? (rest others))
                                       (arbitrary (first others))
                                       (arbitrary/arbitrary-mixed (into {} (map (fn [realm]
                                                                                  ;; Note: arbitrary-mixed doesn't actually use the predicate, yet
                                                                                  [(realm-inspection/predicate realm) (arbitrary realm)])
                                                                                others))))
                                     (apply every-pred (concat (map #(fn [v] (realm/contains? % v)) others)
                                                               (map realm-inspection/predicate preds)))))))
          
          (realm-inspection/from-predicate? realm)
          ;; This will likely not be able to generate any values (as 'any' cannot generate
          ;; everything), but we have no chance but to give it a try:
          (arbitrary/such-that arbitrary/arbitrary-any (realm-inspection/predicate realm))
          
          (realm-inspection/function? realm)
          ;; ...this'll need coarbitraries for everything, doesn't it?
          (throw (unsupported-realm-execption `realm/function))
          
          :else
          (throw (unsupported-realm-execption realm))))))
