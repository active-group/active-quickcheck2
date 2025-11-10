(ns active.quickcheck2.realm-test
  (:require [active.data.realm :as realm]
            [active.quickcheck2 :as qc]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(defn check-quick
  [prop]
  (let [[_ntests _stamps success] (qc/quickcheck-results prop)]
    (true? success)))

(t/deftest arbitrary-test
  (t/testing "uuid"
    (t/is (check-quick (qc/property [uuid realm/uuid]
                           (uuid? uuid)))))
  (t/testing "number"
    (t/is (check-quick (qc/property [n realm/number]
                           (or (integer? n)
                               (double? n))))))
  (t/testing "integer-from-to"
    (t/is (check-quick (qc/property [i (realm/integer-from-to 0 100)]
                           (and (>= i 0) (<= i 100))))))
  (t/testing "optional"
    (t/is (check-quick (qc/property [opt-i (realm/optional realm/integer)]
                           (or (nil? opt-i) (integer? opt-i))))))
  (t/testing "union"
    (t/is (check-quick (qc/property [u (realm/union realm/boolean realm/integer)]
                           (or (boolean? u) (integer? u))))))
  (t/testing "set"
    (t/is (check-quick (qc/property [s (realm/set-of realm/integer)]
                           (and (set? s) (every? integer? s))))))
  (t/testing "enum"
    (t/is (check-quick (qc/property [elem (realm/tuple realm/integer realm/string)]
                           (and (integer? (first elem)) (string? (second elem)))))))
  (t/testing "tuple"
    (t/is (check-quick (qc/property [[i b] (realm/tuple realm/integer realm/boolean)]
                           (and (integer? i) (boolean? b))))))
  (t/testing "named produces the same typed values as its underlying realm"
    (t/is (check-quick (qc/property [n (realm/named "some name" realm/integer)]
                           (integer? n)))))
  (t/testing "map-of"
    (t/is (check-quick (qc/property [m (realm/map-of realm/keyword realm/integer)]
                           (and (every? keyword? (keys m))
                                (every? integer? (vals m)))))))
  
  )

;; missing
;; - delayed
;; - intersection
;; - function
;; - any
