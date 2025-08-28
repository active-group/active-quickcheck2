(ns active.quickcheck2.realm
  (:require [active.data.realm :as realm]
            [active.data.realm.inspection :as realm-inspection]
            [active.quickcheck2.arbitrary :as arbitrary]))

(def arbitrary-key ::arbitrary)

(defn with-arbitrary
  [realm arbitrary]
  (realm/with-metadata arbitrary-key arbitrary))

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

        (cond
          (realm-inspection/sequence-of? realm)
          (arbitrary/arbitrary-list (arbitrary (realm-inspection/sequence-of-realm-realm realm)))))))
          
