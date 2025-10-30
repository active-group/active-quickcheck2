(ns active.quickcheck2.realm-test
  (:require [active.quickcheck2.realm :as sut]
            [active.data.realm :as realm]
            [active.quickcheck2 :as qc]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(defn check-quick
  [prop]
  (let [[_ntests _stamps success] (qc/quickcheck-results prop)]
    success))

(t/deftest arbitrary-test
  (t/testing "tuple"
    (t/is (true? (check-quick
                  (qc/property [[i b] (realm/tuple realm/integer realm/boolean)]
                      (and (integer? i) (boolean? b)))))))
  (t/testing "uuid"
    (t/is (true? (check-quick
                  (qc/property [uuid realm/uuid]
                      (uuid? uuid)))))))

