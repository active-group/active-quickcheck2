(ns active.quickcheck2.realm
  (:require [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]
            [active.quickcheck2.generator-applicative :as generator-applicative]
            [active.quickcheck2.generator :as generator]
            [active.quickcheck2.arbitrary :as arbitrary])
  #?(:clj (:import (java.lang UnsupportedOperationException)
                   (java.util UUID))))

(def arbitrary-key ::arbitrary)

(defn with-arbitrary
  [realm arbitrary]
  (realm/with-metadata realm arbitrary-key arbitrary))

(defn- unsupported-realm-execption [realm]
  (let [message (str "arbitrary not implemented for realm " realm)]
    #?(:clj  (UnsupportedOperationException. message)
       :cljs (js/Error. message))))

(def arbitrary-uuid
  (arbitrary/make-arbitrary
   (generator-applicative/generator-map
    (fn [s]
      (let [bytes* #?(:clj  (.getBytes s)
                      :cljs (let [utf8-encode (js/TextEncoder.)]
                              (.encode utf8-encode s)))]
        (UUID/nameUUIDFromBytes bytes*)))
    (generator/choose-string generator/choose-alphanumeric-char 10))))

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
          (arbitrary/generate-one-of (map arbitrary (realm-inspection/union-realm-realms realm)))
          
          (realm-inspection/set-of? realm)
          (arbitrary/arbitrary-set (arbitrary (realm-inspection/set-of-realm-realm realm)))
          
          (realm-inspection/enum? realm)
          ;; TODO: Is `=` really enough? There is no explicit constraint on the
          ;; equality of enum-values in realms, so this might be okay.
          (arbitrary/make-arbitrary (generator/choose-one-of (realm-inspection/enum-realm-values realm)))

          (realm-inspection/tuple? realm)
          (apply arbitrary/arbitrary-tuple (map arbitrary (realm-inspection/tuple-realm-realms realm)))

          (realm-inspection/named? realm)
          (arbitrary (realm-inspection/named-realm-realm realm))

          (realm-inspection/delayed? realm)
          (arbitrary (realm-inspection/delayed-realm realm))

          (realm-inspection/map-of? realm)
          (arbitrary/arbitrary-map (arbitrary (realm-inspection/map-of-realm-key-realm realm))
                                   (arbitrary (realm-inspection/map-of-realm-value-realm realm)))

          (realm-inspection/function? realm)
          (let [function-cases (realm-inspection/function-realm-cases realm)
                arbitrary-result (arbitrary (realm-inspection/function-case-return-realm realm))]
            (arbitrary/arbitrary-function arbitrary-result ))

          ;; TODO
          (realm-inspection/intersection? realm) ; NOTE: `restricted` realms
                                        ; are also intersections.
          (throw (unsupported-realm-execption `realm/intersection))))))
