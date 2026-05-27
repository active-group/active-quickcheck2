(ns active.quickcheck2.generator-test
  (:require [clojure.test :refer :all]
            [active.quickcheck2.generator-applicative :refer [integrated]]
            [active.quickcheck2.random :as random]
            [active.quickcheck2.generator :refer :all]
            [active.quickcheck2.tree :as tree]))

(def test-gen (random/make-random-generator 12))

(defn test-generate [m] (generate 5 test-gen m 20))

(deftest choose-int-works
  (testing "choose-int produces tree of ints"
    (is (tree/approx-valid-tree? 5 (test-generate choose-int)))
    (is (every? int? (take 100 (tree/to-list (test-generate choose-int)))))))

(deftest choose-byte-works
  (testing "choose-byte produces tree of byte"
    (is (tree/approx-valid-tree? 5 (test-generate choose-byte)))))

(deftest choose-ascii-char-works
  (testing "choose-ascii-char produces tree of chars"
    (is (tree/approx-valid-tree? 5 (test-generate choose-ascii-char)))
    (is (every? char? (take 100 (tree/to-list (test-generate choose-ascii-char)))))))

(deftest choose-non-numeric-char-works
  (testing "choose-non-numeric-char produces tree of chars"
    (is (tree/approx-valid-tree? 5 (test-generate choose-non-numeric-char)))
    (is (every? char? (take 100 (tree/to-list (test-generate choose-non-numeric-char)))))))

(deftest choose-char-works
  (testing "choose-char produces tree of chars"
    (is (tree/approx-valid-tree? 5 (test-generate (choose-char 10 24))))
    (is (every? char? (take 100 (tree/to-list (test-generate (choose-char 0 20))))))))

(deftest choose-list-works
  (testing "choose-list produce tree of lists"
    (is (tree/approx-valid-tree? 5 (test-generate (choose-list choose-int 0))))
    (is (every? coll? (take 100 (tree/to-list (test-generate (choose-list choose-int 2))))))
    (is (every? (partial every? int?) (take 100 (tree/to-list (test-generate (choose-list choose-int 2))))))
    (is (tree/approx-valid-tree? 5 (test-generate (choose-list choose-int 4))))
    (is (every? coll? (take 100 (tree/to-list (test-generate (choose-list choose-int 3))))))))

(deftest choose-one-of-works
  (testing "choose-one-of produces tree of given stuff"
    (is (tree/approx-valid-tree? 5 (test-generate (choose-one-of (seq "abc")))))
    (is (every? char? (take 100 (tree/to-list
                                 (test-generate (choose-one-of (seq "abc")))))))))

(nth (list (choose-char 0 1)) 0)

(deftest choose-mixed-works
  (testing "choose-mixed produces produces tree of mixed"
    (is (tree/approx-valid-tree? 5 (test-generate
                                     (choose-mixed (list choose-non-numeric-char
                                                         (choose-char 0 1))))))
    (is (every? char? (take 100 (tree/to-list
                                 (test-generate
                                   (choose-mixed (list choose-non-numeric-char
                                                        (choose-char 0 1))))))))))

(deftest choose-symbol-works
  (testing "choose-symbol produces tree of symbols"
    (is (tree/approx-valid-tree? 5 (test-generate (choose-symbol 2))))
    (is (every? symbol? (take 100 (tree/to-list (test-generate (choose-symbol 2))))))))

(deftest such-that-works
  (testing "such-that-generator works"
    (is (every? even? (take 100 (tree/to-list (test-generate (such-that-generator choose-int even?)))))))
  (testing "such-that-maybe"
    ;; Note: actually not sure how such-that-maybe is supposed to work
    (is (nil? (test-generate (such-that-maybe choose-int #(= "foo" %)))))
    #_(is (empty? (take 1 (tree/to-list (test-generate (such-that-maybe choose-int #(= "foo" %)))))))))
