(ns active.quickcheck2.arbitrary-test
  (:require [clojure.test :refer :all]
            [active.quickcheck2.random :as random]
            [active.quickcheck2.generator :refer [generate]]
            [active.quickcheck2.tree :as tree]
            [active.quickcheck2.arbitrary :refer :all]))

(def test-gen (random/make-random-generator 12))

(defn test-generate [m] (generate 5 test-gen m 20))

(deftest arbitrary-sequence-like-works
  (testing "arbitrary-sequ-like produces tree of sequence"
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-generator
                                     (arbitrary-sequence-like list
                                                              arbitrary-integer)))))
    (is (every? list? (take 100 (tree/to-list
                                 (test-generate
                                  (arbitrary-generator
                                   (arbitrary-sequence-like list
                                                            arbitrary-integer)))))))
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-generator
                                     (arbitrary-sequence-like vec
                                                              arbitrary-integer)))))
    (is (every? vector? (take 100 (tree/to-list
                                   (test-generate
                                    (arbitrary-generator
                                     (arbitrary-sequence-like vec
                                                              arbitrary-integer)))))))))

(deftest arbitrary-list-works
  (testing "arbitrary-sequ-like produces tree of sequence"
    (is (tree/approx-valid-tree? 5 (test-generate
                                    (arbitrary-generator
                                     (arbitrary-list arbitrary-integer)))))
    (is (every? list? (take 100 (tree/to-list
                                 (test-generate
                                  (arbitrary-generator
                                   (arbitrary-list arbitrary-integer)))))))
    (is (every? (partial every? int?) (take 100 (tree/to-list
                                                 (test-generate
                                                  (arbitrary-generator
                                                   (arbitrary-list arbitrary-integer)))))))))
