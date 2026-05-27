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

(def ^:private coarbitrary-key ::coarbitrary)

(defn with-arbitrary
  [realm arbitrary & [coarbitrary]]
  (cond-> realm
    true (realm/with-metadata arbitrary-key arbitrary)
    (some? coarbitrary) (realm/with-metadata coarbitrary-key coarbitrary)))

(defn- unsupported-exception [^String message]
  #?(:clj  (UnsupportedOperationException. message)
     :cljs (js/Error. message)))

(defn- unsupported-realm-execption [realm]
  (let [message (str "arbitrary not implemented for realm " realm)]
    (unsupported-exception message)))

(defn- arbitrary-map-with-keys [m]
  (apply arbitrary/arbitrary-record (fn [& vs]
                                      (zipmap (keys m) vs))
         (keys m)
         (vals m)))

(defn- coarbitrary-map-with-keys [m]
  (apply arbitrary/coarbitrary-record (fn [& vs]
                                        (zipmap (keys m) vs))
         (keys m)
         (vals m)))

(declare coarbitrary)

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
        realm/uuid arbitrary/arbitrary-uuid
        realm/any arbitrary/arbitrary-any
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
          (arbitrary/arbitrary-one-of = (realm-inspection/enum-realm-values realm))

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
          (apply arbitrary/arbitrary-record (realm-inspection/record-realm-constructor realm)
                 (map realm-inspection/record-realm-field-getter (realm-inspection/record-realm-fields realm))
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
          (if (> (count (realm-inspection/function-realm-cases realm)) 1)
            (throw (unsupported-exception "Arbitrary functions with multiple cases are not supported yet."))
            (let [c (first (realm-inspection/function-realm-cases realm))]
              (if (some? (realm-inspection/function-case-optional-arguments-realm c))
                (throw (unsupported-exception "Arbitrary functions with optional arguments are not supported yet."))
                (apply arbitrary/arbitrary-function (arbitrary (realm-inspection/function-case-return-realm c))
                       (map coarbitrary (realm-inspection/function-case-positional-argument-realms c))))))
          
          :else
          (throw (unsupported-realm-execption realm))))))

(defn coarbitrary
  [realm]
  (or (get (realm-inspection/metadata realm) coarbitrary-key)
      (condp = realm
        realm/natural arbitrary/coarbitrary-natural
        realm/integer arbitrary/coarbitrary-integer
        realm/rational arbitrary/coarbitrary-rational
        realm/real arbitrary/coarbitrary-float
        realm/char arbitrary/coarbitrary-char
        realm/string arbitrary/coarbitrary-string
        realm/symbol arbitrary/coarbitrary-symbol
        realm/keyword arbitrary/coarbitrary-keyword
        realm/boolean arbitrary/coarbitrary-boolean
        realm/uuid arbitrary/coarbitrary-uuid
        realm/any arbitrary/coarbitrary-any
        realm/number (arbitrary/coarbitrary-mixed [[integer? arbitrary/coarbitrary-integer]
                                                   [double? arbitrary/coarbitrary-float]
                                                   #?(:clj [#(instance? java.lang.Long %) arbitrary/coarbitrary-long])])

        (cond
          (realm-inspection/sequence-of? realm)
          (arbitrary/coarbitrary-list (coarbitrary (realm-inspection/sequence-of-realm-realm realm)))

          (realm-inspection/integer-from-to? realm)
          (arbitrary/coarbitrary-integer-from-to (realm-inspection/integer-from-to-realm-from realm)
                                                 (realm-inspection/integer-from-to-realm-to realm))

          (realm-inspection/optional? realm)
          (arbitrary/coarbitrary-mixed [[nil? (arbitrary/coarbitrary-one-of = nil)]
                                        [some? (coarbitrary (realm-inspection/optional-realm-realm realm))]])
          
          (realm-inspection/union? realm)
          (arbitrary/coarbitrary-mixed (mapv (fn [realm]
                                               [(realm-inspection/predicate realm) (coarbitrary realm)])
                                             (realm-inspection/union-realm-realms realm)))
          
          (realm-inspection/set-of? realm)
          (arbitrary/coarbitrary-set (coarbitrary (realm-inspection/set-of-realm-realm realm)))
          
          (realm-inspection/enum? realm)
          (arbitrary/coarbitrary-one-of (realm-inspection/enum-realm-values realm))

          (realm-inspection/tuple? realm)
          (apply arbitrary/coarbitrary-tuple (map coarbitrary (realm-inspection/tuple-realm-realms realm)))

          (realm-inspection/named? realm)
          (coarbitrary (realm-inspection/named-realm-realm realm))

          (realm-inspection/delayed? realm)
          (coarbitrary @(realm-inspection/delayed-realm-delay realm))

          (realm-inspection/map-of? realm)
          (arbitrary/coarbitrary-map (coarbitrary (realm-inspection/map-of-realm-key-realm realm))
                                     (coarbitrary (realm-inspection/map-of-realm-value-realm realm)))

          (realm-inspection/map-with-keys? realm)
          (let [m (realm-inspection/map-with-keys-realm-map realm)]
            (coarbitrary-map-with-keys (zipmap (keys m) (map coarbitrary (vals m)))))

          (realm-inspection/map-with-tag? realm)
          (arbitrary/coarbitrary-map (arbitrary/coarbitrary-one-of = (realm-inspection/map-with-tag-realm-key realm))
                                     (coarbitrary (realm-inspection/map-with-tag-realm-value )))

          (realm-inspection/record? realm)
          (apply arbitrary/coarbitrary-record (realm-inspection/record-realm-constructor realm)
                 (map realm-inspection/record-realm-field-getter (realm-inspection/record-realm-fields realm))
                 (map coarbitrary (map realm-inspection/record-realm-field-realm (realm-inspection/record-realm-fields realm))))

          (realm-inspection/intersection? realm)
          (throw (unsupported-realm-execption `realm/intersection))
          ;; TODO: co-such-that or what?
          #_(let [realms (realm-inspection/intersection-realm-realms realm)]
              ;; for a slight performance inprovement (but a common case via 'realm/restricted'), try to separate from-predicate realms and others:
              (let [{others false preds true} (group-by (comp boolean realm-inspection/from-predicate?) realms)]
                (if (empty? others)
                  ;; only predicate realms? This will likely not be able to generate any
                  ;; values (as 'any' cannot generate everything), but we have no chance but to give it a try:
                  (arbitrary/co-such-that arbitrary/coarbitrary-any
                                          (apply every-pred (concat (map realm-inspection/predicate preds))))
                  ;; use all non-predicate realms 'in turn'; this might lead to slightly
                  ;; better runtime behaviour than using just one as the basis:
                  (arbitrary/co-such-that (if (empty? (rest others))
                                            (coarbitrary (first others))
                                            (arbitrary/coarbitrary-mixed (into {} (map (fn [realm]
                                                                                         [(realm-inspection/predicate realm) (coarbitrary realm)])
                                                                                       others))))
                                          (apply every-pred (concat (map #(fn [v] (realm/contains? % v)) others)
                                                                    (map realm-inspection/predicate preds)))))))
          
          (realm-inspection/from-predicate? realm)
          ;; This will likely not be able to generate any values (as 'any' cannot generate
          ;; everything), but we have no chance but to give it a try:
          (arbitrary/such-that arbitrary/coarbitrary-any (realm-inspection/predicate realm))
          
          (realm-inspection/function? realm)
          (if (> (count (realm-inspection/function-realm-cases realm)) 1)
            (throw (unsupported-exception "Coarbitrary functions with multiple cases are not supported yet."))
            (let [c (first (realm-inspection/function-realm-cases realm))]
              (if (some? (realm-inspection/function-case-optional-arguments-realm c))
                (throw (unsupported-exception "Coarbitrary functions with optional arguments are not supported yet."))
                (apply arbitrary/coarbitrary-function (coarbitrary (realm-inspection/function-case-return-realm c))
                       (map arbitrary (realm-inspection/function-case-positional-argument-realms c))))))
          
          :else
          (throw (unsupported-realm-execption realm))))))
