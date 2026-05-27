(ns active.quickcheck2.arbitrary-test
  (:require [clojure.test :refer :all]
            [active.quickcheck2.random :as random]
            [active.quickcheck2.generator :refer [generate]]
            [active.quickcheck2.tree :as tree]
            [active.quickcheck2.arbitrary :refer :all]))

(def test-gen (random/make-random-generator 12))

(defn test-generate [arb]
  (generate 5 test-gen (arbitrary-generator arb) 20))

(defn gen [n arb]
  (take n (tree/to-list (test-generate arb))))

(deftest arbitrary-sequence-like-works
  (testing "arbitrary-sequ-like produces tree of sequence"
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-sequence-like list
                                                             arbitrary-integer))))
    (is (every? list? (gen 100 (arbitrary-sequence-like list
                                                        arbitrary-integer))))
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-sequence-like vec
                                                             arbitrary-integer))))
    (is (every? vector? (gen 100 (arbitrary-sequence-like vec
                                                          arbitrary-integer))))))

(deftest arbitrary-list-works
  (testing "arbitrary-sequ-like produces tree of sequence"
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-list arbitrary-integer))))
    (is (every? list? (gen 100 (arbitrary-list arbitrary-integer))))
    (is (every? (partial every? int?) (gen 100 (arbitrary-list arbitrary-integer))))))
