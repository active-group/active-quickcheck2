(ns active.quickcheck2.generator-applicative-test
  (:require [clojure.test :refer :all]
            [active.quickcheck2.random :as random]
            [active.clojure.monad :as monad]
            [active.quickcheck2.generator :as generator]
            [active.quickcheck2.tree :as tree])
  (:use active.quickcheck2.generator-applicative))

(def test-gen (random/make-random-generator 12))

(defn generate [m] (generator/generate 5 test-gen m 20))

(deftest generator-map-works
  (testing "generator-map works"
    (is (= (tree/lazy-tree 1 []) (generate (generator-map (partial + 1)
                                                          (monad/return (tree/lazy-tree 0 []))))))))

(deftest generator-apply-works
  (testing "applying of geneator gives a new generator with trees in it"
    (is (tree/valid-tree? (generate (generator-apply (monad/return (tree/lazy-tree (partial + 1) []))
                                                     (monad/return (tree/lazy-tree 0 []))))))))

(defn numshrink
  [x]
  (cond (= x 0) []
        (> x 0) [ (quot x 2) (- x 1)]
        :else [(* x 2) (+ x 1)]))

(deftest integrated-works
  (testing "integrated returns an valid tree"
    (is (tree/valid-tree? (generate (integrated numshrink (monad/return 5)))))))

(deftest combine-generators-curry-works
  (testing "compining of geneator gives a new generator with trees in it"
    (is (tree/valid-tree? (generate (combine-generators-curry vector
                                                              (integrated numshrink (monad/return 5))))))
    (is (tree/valid-tree? (generate (combine-generators-curry (fn [a] (fn [b] (fn [c] (vector a b c))))
                                                              (integrated numshrink (monad/return 1))
                                                              (integrated numshrink (monad/return 3))
                                                              (integrated numshrink (monad/return 4))))))
    (is (tree/valid-tree? (generate (combine-generators-curry (curry vector 2)
                                                              (monad/return (tree/lazy-tree 1 []))
                                                              (monad/return (tree/lazy-tree 2 []))))))
    (is (tree/valid-tree? (generate (combine-generators-curry (curry vector 2)
                                                              (monad/return (tree/lazy-tree 'a []))
                                                              (monad/return (tree/lazy-tree 'b []))))))))

(deftest combine-generators-works
  (testing "compining of geneator gives a new generator with trees in it"
    (is (tree/valid-tree? (generate (combine-generators vector))))
    (is (tree/valid-tree? (generate (combine-generators vector
                                                        (integrated numshrink (monad/return 5))))))
    (is (tree/valid-tree? (generate (combine-generators vector
                                                        (monad/return (tree/lazy-tree 1 []))
                                                        (monad/return (tree/lazy-tree 2 []))))))
    (is (tree/valid-tree? (generate (combine-generators vector
                                                        (monad/return (tree/lazy-tree 'a []))
                                                        (monad/return (tree/lazy-tree 'b []))))))
    (is (tree/valid-tree? (generate (combine-generators list
                                                        (monad/return (tree/lazy-tree 'a []))
                                                        (monad/return (tree/lazy-tree 'b []))))))
    (is (tree/approx-valid-tree? 3 (generate (combine-generators list
                                                                 (integrated numshrink (monad/return 1))
                                                                 (integrated numshrink (monad/return 3))
                                                                 (integrated numshrink (monad/return 4))))))))

