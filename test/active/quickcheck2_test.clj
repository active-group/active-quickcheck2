(ns active.quickcheck2-test
  (:require [clojure.test :refer :all]
            [active.clojure.monad :as monad]
            [active.quickcheck2.generator :as generator]
            [active.quickcheck2.generator-applicative :refer [integrated]]
            [active.quickcheck2.tree :as tree]
            [active.quickcheck2.random :as random]
            [active.quickcheck2.generator :refer [generate]]
            [active.quickcheck2.arbitrary :refer [arbitrary-integer arbitrary-natural]]
            [active.data.realm :as realm]
            [clojure.math.numeric-tower :refer [expt]])
  (:use active.quickcheck2))

(defn check-quick
  [prop]
  (let [[ntests stamps success] (quickcheck-results prop)]
    success))

(deftest ok
  (testing "trivial property"
    (is
     (quickcheck
      (property [x realm/integer]
                (= x x))))
    (is
     (quickcheck
      (property [x realm/char]
                (= x x))))))

(deftest ok-multible-arguments
  (testing "trivaial property with multible arguments"
    (is
     (quickcheck
      (property [x realm/integer
                 y realm/integer
                 z realm/integer]
                (and (= x x ) (= y y) (= z z)))))))

(deftest not-ok
  (testing "trivial property"
    (is
     (not= true
           (check-quick
            (property [x realm/integer]
                      (not= x x)))))))

(deftest sometimes-ok
  (testing "trivial property"
    (is
     (not= true
           (check-quick
            (property [x realm/integer]
                      (< x 5)))))))

(deftest ok-list
  (testing "trivial property for list"
    (is
     (quickcheck
      (property [xs (realm/sequence-of realm/integer)]
                (= xs xs))))))

(deftest reverse-distributes-over-concat
  (testing "reverse distributes over concat"
    (is
     (quickcheck
      (property [xs (realm/sequence-of realm/integer)
                 ys (realm/sequence-of realm/integer)]
                (= (reverse (concat xs ys)) (concat (reverse ys) (reverse xs))))))))

(deftest reverse-distributes-over-concat-broken
  (testing "reverse distributes over concat: broken"
    (is
     (not= true
           (check-quick
            (property [xs (realm/sequence-of realm/integer)
                       ys (realm/sequence-of realm/integer)]
                      (= (reverse (concat ys xs)) (concat (reverse ys) (reverse xs)))))))))

(deftest clojure-symbol
  (testing "clojure symbol"
    (is
     (quickcheck
      (property [x realm/symbol]
                (symbol? x))))))

(deftest huge-integer
  (testing "trivial property"
    (is
     (not= true
           (check-quick
            (property [x realm/integer]
                      (< x (expt 2 40))))))))

(deftest natural
  (testing "arbitrary-natural generates only natural numbers"
    (is
     (quickcheck
      (property [x realm/natural]
                (and (integer? x)
                     (>= x 0)))))))

(deftest rational
  (testing "arbitrary-rational generates only rational numbers"
    (is
     (quickcheck
      (property [x realm/rational]
                (rational? x))))))

(deftest resizeq
  (testing "whether resize works"
    (is
     (quickcheck
      (property [x (generator/resize 100 (coerce->generator realm/natural))]
                (and (integer? x)
                     (>= x 0)
                     (<= x 100)))))))

(deftest floatq
  (testing "arbitrary-float generates only floats"
    (is
     (quickcheck
      (property [x realm/real]
                (float? x))))))

(deftest charq
  (testing "arbitrary-char generates only chars"
    (is
     (quickcheck
      (property [x realm/char]
                (char? x))))))

;; (deftest ascii-char
;;   (testing "arbitrary-ascii-char generates only ASCII chars."
;;     (is
;;      (quickcheck
;;       (property [x ascii-char]
;;                 (and (char? x)
;;                      (< (int x) 128)))))))

;; (deftest byteq
;;   (testing "arbitrary-byte generates only shorts."
;;     (is
;;      (quickcheck
;;       (property [x byte]
;;                 (and (integer? x)
;;                      (instance? java.lang.Byte x)
;;                      (>= x -128)
;;                      (<= x 127)))))))

;; (deftest unsigned-byteq
;;   (testing "arbitrary-unsigned-byte generates only unsigned bytes."
;;     (is
;;      (quickcheck
;;       (property [x unsigned-byte]
;;                 (and (integer? x)
;;                      (instance? java.lang.Short x)
;;                      (>= x 0)
;;                      (<= x 255)))))))

;; (deftest shortq
;;   (testing "arbitrary-short generates only shorts."
;;     (is
;;      (quickcheck
;;       (property [x short]
;;                 (and (integer? x)
;;                      (instance? java.lang.Short x)
;;                      (>= x -32768)
;;                      (<= x 32767)))))))

;; (deftest unsigned-shortq
;;   (testing "arbitrary-unsigned-short generates only unsigned shorts."
;;     (is
;;      (quickcheck
;;       (property [x unsigned-short]
;;                 (and (integer? x)
;;                      (instance? java.lang.Integer x)
;;                      (>= x 0)
;;                      (<= x 65535)))))))

;; (deftest intq
;;   (testing "arbitrary-long generates only longs."
;;     (is
;;      (quickcheck
;;       (property [x int]
;;                 (and (integer? x)
;;                      (int? x)
;;                      (>= x -2147483648)
;;                      (<= x 2147483647)))))))

;; (deftest unsigned-intq
;;   (testing "arbitrary-unsigned-int generates only unsigned ints."
;;     (is
;;      (quickcheck
;;       (property [x unsigned-int]
;;                 (and (integer? x)
;;                      (int? x)
;;                      (>= x 0)
;;                      (<= x 4294967295)))))))

;; (deftest longq
;;   (testing "arbitrary-long generates only longs."
;;     (is
;;      (quickcheck
;;       (property [x long]
;;                 (and (integer? x)
;;                      (instance? java.lang.Long x)
;;                      (>= x -9223372036854775808)
;;                      (<= x 9223372036854775807)))))))

;; (deftest unsigned-longq
;;   (testing "arbitrary-unsigned-long generates only unsigned longs."
;;     (is
;;      (quickcheck
;;       (property [x unsigned-long]
;;                 (and (integer? x)
;;                      (>= x 0)
;;                      (<= x 18446744073709551615)))))))

;; (deftest integer-from-toq
;;   (testing "arbitrary-integer-from-to generates only longs."
;;     (is
;;      (quickcheck
;;       (property 
;;        [a integer
;;         b integer]
;;        (let [from (min a b)
;;              to (max a b)]
;;        (property
;;         [x (integer-from-to from to)]
;;         (and (integer? x)
;;              (>= x from)
;;              (<= x to)))))))))

(deftest stringq
  (testing "arbitrary-string generates only strings."
    (is
     (quickcheck
      (property [x realm/string]
                (string? x))))))

;; (deftest byte-arrayq
;;   (testing "arbitrary-byte-array generates only byte arrays."
;;     ;; http://stackoverflow.com/questions/14796964/how-to-check-if-a-clojure-object-is-a-byte-array
;;     (let [check (type (byte-array []))]
;;       (is
;;        (quickcheck
;;         (property [x byte-array]
;;                   (instance? check x)))))))

(deftest symbolq
  (testing "arbitrary-symbol generates only symbols."
    (is
     (quickcheck
      (property [x realm/symbol]
                (symbol? x))))))

(defn valid-symbol-name?
  [name]
  ;; read-string knows the rules
  (try
    (= (keyword name) (read-string (str ":" name)))
    (catch RuntimeException e
      ;; EOF while reading, if not a proper s-expr
      false)))

(deftest symbol-valid
  (testing "arbitrary-symbol generates valid symbols."
    (is
     (quickcheck
      (property [x realm/symbol]
                (and (symbol? x) (valid-symbol-name? (name x))))))))

(deftest keywordq
  (testing "arbitrary-keyword generates only keywords."
    (is
     (quickcheck
      (property [x realm/keyword]
                (keyword? x))))))

(deftest keyword-valid
  (testing "arbitrary-keyword generates valid keywords."
    (is
     (quickcheck
      (property [x realm/keyword]
                ;; same rules as for symbols..
                (and (keyword? x) (valid-symbol-name? (name x))))))))

;; (deftest ascii-string
;;   (testing "arbitrary-ascii-string generates only ASCII strings"
;;     (is
;;      (quickcheck
;;       (property [x ascii-string]
;;                 (and (string? x)
;;                      (every? #(< (int %) 128) x)))))))

;; (deftest mixed
;;   (testing "arbitrary-mixed works"
;;     (is
;;      (quickcheck
;;       (property [x (mixed integer? integer
;;                           string? string)]
;;                 (or (integer? x) (string? x)))))))

;; (deftest one-of
;;   (testing "arbitrary-one-of works"
;;     (is
;;      (quickcheck
;;       (property [x (one-of = "foo" "bar" "baz")]
;;                 (contains? #{"foo" "bar" "baz"} x))))))

;; (deftest listq
;;   (testing "arbitrary-list works"
;;     (is
;;      (quickcheck
;;       (property [x (list integer)]
;;                 (and (list? x)
;;                      (every? integer? x)))))))

;; (deftest vectorq
;;   (testing "arbitrary-vector works"
;;     (is
;;      (quickcheck
;;       (property [x (vector integer)]
;;                 (and (vector? x)
;;                      (every? integer? x)))))))

;; (deftest setq
;;   (testing "arbitrary-set works"
;;     (is
;;      (quickcheck
;;       (property [x (set integer)]
;;                 (and (set? x)
;;                      (every? integer? x)))))))

;; (deftest mapq
;;   (testing "arbitrary-map works"
;;     (is
;;      (quickcheck
;;       (property [x (map integer string)]
;;                 (and (map? x)
;;                      (every? integer? (keys x))
;;                      (every? string? (vals x))))))))

;; (deftest boolean-function
;;   (testing "creating a function bool -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc (boolean -> integer)]
;;                 (generator/function-memorize? proc))))
;;     (is
;;      (quickcheck
;;       (property [proc (boolean -> integer)]
;;                      (integer? (proc true)))))))

;; (deftest integer-function
;;   (testing "creating a function int -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc (integer -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc 17))))))))

;; (deftest natural-function
;;   (testing "creating a function nat -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc (integer -> natural)]
;;                 (and (generator/function-memorize? proc)
;;                      (and (integer? (proc 17)))))))))



;; (deftest rational-function
;;   (testing "creating a function rational -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc (rational -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc 2/3))))))))

;; (deftest real-function
;;   (testing "creating a function nat -> int works"
;;     (is
;;       (quickcheck
;;        (property [proc (float -> integer)]
;;                  (and (generator/function-memorize? proc)
;;                       (integer? (proc 17.5))))))))

;; (deftest char-function
;;   (testing "creating a function char -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc (char -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc \a))))))))

;; (deftest ascii-char-function
;;   (testing "creating a function ascii-char -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc (ascii-char -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc \a))))))))
     
;; (deftest string-function
;;   (testing "creating a function string -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc (string -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc "foo"))))))))

;; (deftest ascii-string-function
;;   (testing "creating a function ascii-string -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc (ascii-string -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc "foo"))))))))

;; (deftest symbol-function
;;   (testing "creating a function symbol -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc (symbol -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc 'foo))))))))

;; (deftest mixed-function
;;   (testing "creating a function mixed -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc ((mixed integer? integer
;;                               string?  string)
;;                        -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc 15))
;;                      (integer? (proc "foo"))))))))

;; (deftest one-of-function
;;   (testing "creating a function one-of -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc ((one-of = "foo" "bar" "baz") -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc "foo"))))))))

;; (deftest vector-function
;;   (testing "creating a function [int] -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc ((vector integer) -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc [15 13]))))))))

;; (deftest set-function
;;   (testing "creating a function #{int} -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc ((set integer) -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc #{15 13}))))))))

;; (deftest map-function
;;   (testing "creating a function {:kw, int} -> int works"
;;     (is
;;      (quickcheck
;;       (property [proc ((map keyword integer) -> integer)]
;;                 (and (generator/function-memorize? proc)
;;                      (integer? (proc {:b 0 :a 42}))))))))

;; (deftest function-function
;;   (testing "creating a function function -> int works"
;;     (is
;;       (quickcheck
;;        (property [proc ((char -> boolean) -> integer)]
;;                  (and (generator/function-memorize? proc)
;;                       (integer? (proc #(= % \A)))))))))

;; (deftest ==>q
;;   (testing "==> works"
;;     (is
;;      (quickcheck
;;       (property [x integer]
;;                 (==> (even? x)
;;                      (integer? (/ x 2))))))))

;; (deftest labelq
;;   (testing "label works"
;;     (is
;;      (let [[ntests stamps success]
;;            (quickcheck-results (property [x integer]
;;                                          (label "yo" (integer? x))))]
;;        (and (true? success)
;;             (every? (fn [el]
;;                       (= el '("yo")))
;;                     stamps))))))

;; (deftest classifyq
;;   (testing "classify works"
;;     (is
;;      (let [[ntests stamps success]
;;            (quickcheck-results (property [x integer]
;;                                          (classify (even? x) "even" (integer? x))))]
;;        (and (true? success)
;;             (every? (fn [el]
;;                       (or (= el [])
;;                           (= el ["even"])))
;;                     stamps))))))

;; (deftest trivialq
;;   (testing "trivial works"
;;     (is
;;      (let [[ntests stamps success]
;;            (quickcheck-results  (property [x integer]
;;                                           (trivial (even? x) (integer? x))))]
;;        (and (true? success)
;;             (every? (fn [el]
;;                       (or (= el [])
;;                           (= el ["trivial"])))
;;                     stamps))))))

;; (defrecord Foo [bar baz])

;; (deftest recordq
;;   (testing "arbitrary-record works"
;;     (is
;;      (quickcheck
;;       (property [x (record ->Foo [:bar integer 
;;                                   :baz string])]
;;                 (and (instance? Foo x) (integer? (:bar x)) (string? (:baz x))))))))

;; (macroexpand '(property [x (record ->Foo [:bar integer 
;;                             :baz string])]
;;           (and (instance? Foo x) (integer? (:bar x)) (string? (:baz x)))))

;; (deftest record2
;;   (testing "arbitrary-record works"
;;     (is
;;       (quickcheck 
;;        (property [proc ((record ->Foo [:bar integer :baz string])
;;                         -> integer)]
;;                  (integer? (proc (Foo. 47 "foo"))))))))

;; (defrecord Bar [bla blu])

;; (deftest record3
;;   (testing "arbitrary-record works"
;;     (is
;;       (quickcheck 
;;        (property [proc ((record ->Bar [:bla integer :blu integer])
;;                         -> integer)]
;;                  (integer? (proc (Bar. 23 42))))))))

;; ;; --- Distribution tools ---------

;; (deftest occurrences-t
;;   (is (= (occurrences [["even" "huge"] ["odd" "huge"] []])
;;          {"even" 1
;;           "odd" 1
;;           "huge" 2})))

;; (deftest distribution-t
;;   (is (= (distribution [["even" "huge"] ["odd" "huge"] []])
;;          {"even" 1/3
;;           "odd" 1/3
;;           "huge" 2/3})))

;; (deftest fraction-of-t
;;   (let [stamps [[] [] []]]
;;     (is (= 0 (fraction-of "no exist" stamps))))

;;   (let [stamps [["even" "huge"] ["odd" "huge"] []]]
;;     (is (= 0 (fraction-of "no exist" stamps)))
;;     (is (= 1/3 (fraction-of "even" stamps)))
;;     (is (= 1/3 (fraction-of "odd" stamps)))
;;     (is (= 2/3 (fraction-of "huge" stamps)))))

;; (deftest distributed-t
;;   (testing "distribution checker works"
;;     (testing "base case"
;;       (let [stamps-1 [["even" "huge"] ["even"] ["odd" "huge"] ["odd"]]]
;;         (is (distributed? stamps-1
;;                           {"even" 0.48
;;                            "odd" 0.48
;;                            "huge" 0.48}))))

;;     (testing "negative case"
;;       (let [stamps-1 [["even" "huge"] ["even"] ["odd" "huge"] ["odd"]]]
;;         (is (not
;;              (distributed? stamps-1
;;                            {"even" 0.52
;;                             "odd" 0.48
;;                             "huge" 0.48})))))

;;     (testing "empty requirements"
;;       (let [stamps-1 [["even"] ["even"] ["odd"] ["odd"]]]
;;         (is (distributed? stamps-1 {}))))

;;     (testing "empty requirements and stamps"
;;       (let [stamps-1 []]
;;         (is (distributed? stamps-1 {}))))

;;     (testing "empty requirements and labels"
;;       (let [stamps-1 [[] [] [] []]]
;;         (is (distributed? stamps-1 {}))))

;;     (testing "multiple"
;;       (let [stamps-1 [["a" "b"] ["a" "b"] ["a" "b"] ["a" "b"]]]
;;         (is (distributed? stamps-1
;;                           {"a" 1
;;                            "b" 1}
;;                           ))))))

;; (deftest with-distribution-t
;;   (testing "some odd some even"
;;     (is

;;      (with-distribution
;;        "even" 0.4
;;        "odd" 0.4

;;        (quickcheck
;;         (property [x integer]
;;                   (label (cond
;;                            (even? x) "even"
;;                            (odd? x) "odd")
;;                          (integer? x))))))))


;; ;; --- Performance ---------

;; ;; `variant` used to be O(n), should now be O(log n)

;; (deftest log-t
;;   (is (= 0 (random/log 1)))
;;   (is (= 1 (random/log 2)))
;;   (is (= 1 (random/log 3)))
;;   (is (= 3 (random/log 15))))

;; (deftest binary-string
;;   (is (= "0" (random/binary-string 0)))
;;   (is (= "1" (random/binary-string 1)))
;;   (is (= "10" (random/binary-string 2)))
;;   (is (= "10000000000000000000000000000000"
;;          (random/binary-string (inc Integer/MAX_VALUE)))))

;; (deftest gamma-encoding
;;   (is (= [\1] (random/gamma-encoding 1)))
;;   (is (= [\0 \1 \0] (random/gamma-encoding 2)))
;;   (is (= [\0 \0 \0 \1 \0 \1 \1] (random/gamma-encoding 11))))

;; (deftest variant-performance-t
;;   (is
;;    (quickcheck
;;     (property [proc (integer -> integer)]
;;               (and (generator/function-memorize? proc)
;;                    (integer? (proc 0))
;;                    (integer? (proc 999999999)))))))

;; (deftest variant-performance-bind-t
;;   (is
;;    (quickcheck
;;     (property [proc ((integer -> integer) -> integer)]
;;               (and (generator/function-memorize? proc)
;;                    (integer? (proc inc))))))

;;   (is
;;    (quickcheck
;;     (property [proc (integer -> (integer -> integer))]
;;               (and (generator/function-memorize? proc)
;;                    (generator/function-memorize? (proc 999999999))
;;                    (integer? ((proc 999999999) 999999999)))))))


;; --- Counter example shrinkage ---------

;; (defn argument-list
;;   [prop]
;;   (second (first (first (check-result-arguments-list (check-quick prop))))))

;; (deftest shrinking-works
;;   (testing "shrinking gives the smallest counterexample"
;;     (is (= [0] (argument-list (property [x realm/integer]
;;                                         (< x x)))))
;;     (is (= [0 0] (argument-list (property [x realm/integer
;;                                            y realm/integer]
;;                                           (< x x)))))
;;     (is (= [0 0] (argument-list (property [x realm/integer
;;                                            y realm/integer]
;;                                           (< x y)))))
;;     (is (= [0 1] (argument-list (property [x realm/natural
;;                                            y realm/natural]
;;                                           (or (> y 1)
;;                                               (= x y))))))
;;     (is (= [[0]] (argument-list (property [xs (realm/sequence-of realm/integer)]
;;                                           (empty? xs)))))
;;     (is (= [[] [0 0 0]] (argument-list
;;                          (property [xs (realm/sequence-of realm/integer)
;;                                     cs (realm/sequence-of realm/natural)]
;;                                    (or (= (count xs) (count cs))
;;                                        (< (count cs) 3)))))))
;;   (testing "after a counterexample is shrunken it is still a counterexample"
;;     (letfn [(prop [xs ys f]
;;             (and (list? xs) (f xs ys) (= (reverse ys) ys)))]
;;       (is
;;        (not
;;         (apply prop (argument-list (property [f ((realm/sequence-of realm/integer) -> boolean)
;;                                                xs (realm/sequence-of realm/integer)
;;                                                ys (realm/sequence-of realm/integer)]
;;                                               (prop xs ys f)))))))))


;; (deftest shrinking-t
;;   (let [counter-list
;;         (-> (property [x (realm/sequence-of realm/integer)]
;;                       (every? even? x))
;;             (check-quick)
;;             (check-result-arguments-list)

;;             well ...
;;             (first)
;;             (first)
;;             (second))]
;;     (is (= 1 (count counter-list)))))

