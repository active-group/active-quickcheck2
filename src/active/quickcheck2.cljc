; Copyright (c) Active Group GmbH. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

; quickcheck.clj: QuickCheck clone for Clojure

(ns ^{:author "Michael Sperber,  Markus Schlegel, Florian Engel, working from John Hughes's original Haskell version"
      :doc "A QuickCheck clone for Clojure."}
  active.quickcheck2
  (:require #?(:clj [active.data.record :refer [def-record is-a?]]
               :cljs [active.data.record :refer [is-a?] :refer-macros [def-record]]))
  (:require [active.clojure.monad :as monad])
  (:require [active.quickcheck2.tree :as tree])
  (:require [active.quickcheck2.generator-applicative :refer [with-tree?
                                                              with-tree
                                                              get-tree
                                                              integrated
                                                              combine-generators
                                                              combine-generators-curry]])
  (:require [active.quickcheck2.generator :as generator])
  (:require [active.quickcheck2.random
             :refer [make-random-generator random-generator-split]])
  (:require [active.quickcheck2.shrink :as shrink])
  (:require [active.quickcheck2.arbitrary :as arbitrary])
  (:require [active.data.realm :as realm])
  (:require [active.data.realm.inspection :as realm-inspection])
  (:require [active.quickcheck2.realm :as quickcheck2-realm])
  (:require [clojure.math.numeric-tower :refer [expt]])
  (:use [clojure.test :only [assert-expr do-report]]))

(def-record ^{:doc "QuickCheck property"}
  Property-type
  [property-func
   property-arg-names
   ;; (seq (union arbitrary generator))
   property-args])

(defn make-property
  [func arg-names args]
  (Property-type property-func func
                 property-arg-names arg-names
                 property-args args))

(defmacro property
  "Create a property through binding identifiers to arbitraries.

The clauses are a vector of alternating identifiers and arbitraries,
which are implicitly in the syntax understood by the arbitrary macro.

The body can use the identifiers, and should evaluate to a boolean
saying whether the property is satisfied."
  [clauses body0 & bodies]
  (when (odd? (count clauses))
    (throw (Exception. "Odd number of elements in property bindings.")))
  (let [pairs (partition 2 clauses)
        ids (map first pairs)
        rhss (map second pairs)]
    `(make-property
       (fn [~@ids]
         ~body0 ~@bodies)
       '~ids
       (list ~@(map (fn [rhs#] `(coerce->generator ~rhs#)) rhss)))))

(def-record ^{:doc "Result from a QuickCheck run."} 
  Check-result-type
  [
   ;; nil = unknown, true, false
   check-result-ok
   check-result-stamp
   ;; (list (list (pair (union #f symbol) value)))
   check-result-arguments-list
   check-result-error-set])

(defn make-check-result
  [ok stamp arguments-list error-set]
  (Check-result-type check-result-ok ok
                     check-result-stamp stamp
                     check-result-arguments-list arguments-list
                     check-result-error-set error-set))

(defn check-result?
  [x]
  (is-a? Check-result-type x))

(defn- result-add-stamp
  [res stamp]
  (assoc res check-result-stamp (conj (check-result-stamp res) stamp)))

; result (list (pair (union #f symbol) value)) -> result
(defn result-add-arguments
  [res args]
  (assoc res check-result-arguments-list
    (conj (check-result-arguments-list res) args)))

(defn result-add-errors
  [res args]
  (assoc res check-result-error-set
         (conj (check-result-arguments-list res) args)))

(defn result-mapped
  "Monoidal plus of result."
  [result1 result2]
  (assert (check-result? result1))
  (assert (check-result? result2))
  (cond
    (check-result-ok result1) result2
    :else result1))

(defn result-add-argument-if-empty
  [res arg]
  (assert (check-result? res))
  (cond
    (empty? (check-result-arguments-list res)) (result-add-arguments res arg)
    :else res))

(def nothing
  (make-check-result nil [] [] #{}))

(defn coerce->generator
  "Coerce an object to a generator."
  [thing]
  (cond

    (is-a? arbitrary/Arbitrary-type thing)
    (arbitrary/arbitrary-generator thing)

    (realm-inspection/realm? thing)
    (arbitrary/arbitrary-generator (quickcheck2-realm/arbitrary thing))

    :else thing))

; A testable value is one of the following:
; - a Property object
; - a boolean
; - a Result record
; - a generator of a Result record
(declare for-all-with-shrink-with-names)

(defn coerce->result-generator
  "Coerce an object to a result generator."
  [thing]
  (cond
    (is-a? Property-type thing) (for-all-with-shrink-with-names (property-func thing)
                                                                    (property-arg-names thing)
                                                                    (property-args thing))
    (instance? Boolean thing) (monad/return (assoc nothing check-result-ok thing))
    (is-a? Check-result-type thing) (monad/return thing)
    :else thing )); 
  
#_(defn for-all
  "Bind names to generated values."
  [func & args]
    (monad/monadic
      [args (monad/sequ (map coerce->generator args))
       res (coerce->result-generator (apply func args))]
      (monad/return (result-add-arguments res
                      (map #(conj % nil) args)))))

#_(defn for-all-with-names
  "Bind names to generated values, supplying informative names."
  [func arg-names args]
  (monad/monadic
    [args (monad/sequ (map coerce->generator args))
     res (coerce->result-generator (apply func args))]
    (monad/return (result-add-arguments res (map list arg-names args)))))

(defn find-failing
  [smaller func]
  (monad/monadic
   [results (monad/sequ
             (mapv coerce->result-generator (mapv (fn [args]
                                                    (try (apply func args)
                                                         (catch
                                                             Exception e
                                                           (make-check-result false
                                                                              []
                                                                              []
                                                                              #{(.toString e)}))))
                                                  (mapv tree/tree-outcome smaller))))]
   (let [failingResults (filter (fn [[_ result]] (not (check-result-ok result)))
                                (mapv vector smaller results))])
   (monad/return
    (cond
      (empty? failingResults) :no-failing-result
      :else (first failingResults)))))

(defn shrinking
  "
  get shrinks of args and find failing result in the resulting list
  recursive call shrinking as long as there is a failing result.
  "
  [arg-names args func fuel]
  (assert (tree/tree? args) "args has to be a tree")
  (let [children (tree/tree-shrinks args)]
    (monad/monadic
     [maybeFailingResult (find-failing children func)]
     (cond
       (or (= maybeFailingResult :no-failing-result)
           (<= fuel 0)) (monad/return (assoc nothing check-result-ok true))
       :else (monad/monadic
              (let [[shrunk, failure] maybeFailingResult])
              [result (shrinking arg-names shrunk func (- fuel 1))]
              (monad/return
               (result-add-argument-if-empty (result-mapped result failure)
                                                       [(vector arg-names (tree/tree-outcome shrunk))])))))))

(defn for-all-with-shrink-with-names
  "Bind name to generated value, try to shrink, supplying informative name.,"
  [func arg-names arg-trees]
  (assert (= (count arg-names) (count arg-trees))
          "Number of arg-names does not match number of arguments")
  (let [arg-trees (map coerce->generator arg-trees)]
    (monad/monadic
      [args-tree (with-tree (apply combine-generators vector arg-trees))
       max-shrink-depth (generator/get-max-shrink-depth)
       res (coerce->result-generator
            (try (apply func (tree/tree-outcome args-tree))
                 (catch Exception e (make-check-result false
                                                       []
                                                       []
                                                       #{(.toString e)}))))]
      (let [result (result-add-arguments res [(vector arg-names (tree/tree-outcome args-tree))])])
      [maybe-shrunken-result (cond
                               (check-result-ok result) (monad/return result)
                               :else (shrinking arg-names args-tree func max-shrink-depth))]
      (monad/return (result-mapped maybe-shrunken-result result)))))

(defmacro ==>
  "Create a property that only has to hold when its prerequisite holds."
  [?bool ?prop]
  `(if ~?bool
     ~?prop
     (monad/return nothing)))

(defn label
  "Label a testable value."
  [str testable]
  (monad/monadic
    [res (coerce->result-generator testable)]
    (monad/return (result-add-stamp res str))))

(defmacro classify
  "Classify some test cases of a testable."
  [?really? ?str ?testable]
  `(let [testable# ~?testable]
     (if ~?really?
       (label ~?str testable#)
       testable#)))

(defmacro trivial
  "Classify some test cases of a testable as trivial."
  [?really? ?testable]
  `(classify ~?really? "trivial" ~?testable))

(defn collect
  "Label a testable value with an the string representation of an object."
  [lbl testable]
  (label (str lbl) testable))

; Running the whole shebang

(def-record ^{:doc "Configuration for a series of QuickCheck test runs."}
  Config-type
  [make-config-max-test
   make-config-max-fail
   make-config-max-shrink-depth
   make-config-size
   make-config-print-every])

(defn make-config
  [max-test max-fail max-shrink-depth size print-every]
  (Config-type
   make-config-max-test max-test
   make-config-max-fail max-fail
   make-config-max-shrink-depth max-shrink-depth
   make-config-size size
   make-config-print-every print-every))

(def quick
  "Quick test-run configuration with minimal output."
  (make-config
    100
    1000
    30
    #(+ 3 (quot % 2))
    (fn [n args] nil)))

(def verbose
  "Quick test-run configuration with verbose output."
  (make-config
    100
    1000
    30
    #(+ 3 (quot % 2))
    (fn [n args]
      (print n)
      (println ":")
      (doseq [x args] (println x)))))

(declare tests)

(defn check-results
  "Run a property against a configuration and return results."
  [config prop]
  (let [rgen (make-random-generator 0)]
    (tests config (coerce->result-generator prop) rgen 0 0 '())))

(declare report-result)
(defn check
  "Run a property against a configuration and report results."
  [config prop]
  (let [[ntest stamps maybe-result]  (check-results config prop)]
    (report-result ntest stamps maybe-result)))

(defn quickcheck-results
  "Run a property against the `quick' configuration and return results."
  [prop]
  (check-results quick prop))

(defn quickcheck
  "Run a property against the `quick' configuration and report results."
  [prop]
  (check quick prop))

#_(def ntries 20)

#_(defn counter-example-of-size [gen size rgen]
  (loop [i ntries
         rgen rgen]
    (when-not (zero? i)
      (let [result (generate size rgen gen 20)
            next-rgen (first (random-generator-split rgen))]
        (case (check-result-ok result)
          true (recur (dec i) next-rgen)
          false result
          )))))

#_(clojure.test/deftest counter-example-of-size-t
  (let [prop (property [x (list integer)]
                       (every? even? x))
        gen (coerce->result-generator prop)
        rgen (make-random-generator 0)
        cex (counter-example-of-size gen 1 rgen)
        ls (-> cex
               (check-result-arguments-list)
               (first) (first) (second))]
    (clojure.test/is (= 1 (count ls)))))

#_(defn smallest-counter-example [gen size example rgen]
  (loop [size (dec size)
         smallest-example example
         rgen (second (random-generator-split rgen))]

    (if (zero? size)
      smallest-example
      (let [new-example (counter-example-of-size gen size rgen)]
        (if new-example
          ;; go smaller
          (recur (dec size) new-example (second (random-generator-split rgen)))
          ;; done
          smallest-example
          )))))

#_(clojure.test/deftest smallest-counter-example-t
  (let [prop (property [x (list integer)]
                       (every? even? x))
        gen (coerce->result-generator prop)
        rgen (make-random-generator 0)
        cex (smallest-counter-example gen 50 :failed rgen)
        ls (-> cex
               (check-result-arguments-list)
               (first) (first) (second))]
    (clojure.test/is (= 1 (count ls)))))

(defn- tests
  "Run a series of test runs.

returns three values:
- ntest
- stamps
- true for success, false for exhausted, result for failure"
  [config gen rgen ntest nfail stamps]
  (loop [rgen rgen
         ntest ntest
         nfail nfail
         stamps stamps]
    (cond
      (= ntest (make-config-max-test config)) (list ntest stamps true)
      (= nfail (make-config-max-fail config)) (list nfail stamps false)
      :else
      (let [[rgen1 rgen2] (random-generator-split rgen)
            size ((make-config-size config) ntest)
            result (generator/generate size rgen2 gen (make-config-max-shrink-depth config))]
        ((make-config-print-every config) ntest (check-result-arguments-list result))
        (case (check-result-ok result)
          nil (recur rgen1 ntest (+ 1 nfail) stamps)
          true (recur rgen1 (+ 1 ntest) nfail (conj stamps (check-result-stamp result)))
          false
          ;; found a counter-example of size size. we now try to
          ;; recursively find a smaller counter-example of size (dec
          ;; size)
          [ntest stamps result]
          #_(let [smallest-result (smallest-counter-example gen size result rgen)]
            [ntest stamps smallest-result]))))))

(declare done write-arguments)

(defn- report-result
  "Report the result of a series of test runs."
  [ntest stamps maybe-result]
  (case maybe-result
    true (done "OK, passed" ntest stamps)
    false (done "Arguments exhausted after" ntest stamps)
    (do
      (print "Falsifiable, after ")
      (print ntest)
      (println " tests:")
      (doseq [a (check-result-arguments-list maybe-result)]
        (write-arguments a))
      (when (not (empty? (check-result-error-set maybe-result)))
        (println "errors:")
        (doseq [error (check-result-error-set maybe-result)]
          (println error))))))

; (pair (union nil symbol) value)
(defn- write-argument
  "Print out an argument binding."
  [arg]
  (when (first arg)
    (print (first arg))
    (print " = "))
  (print (map str (second arg))))

; (list (pair (union nil symbol) value))
(defn- write-arguments
  "Print out a list of argument bindings."
  [args]
  (when (seq args)
    (write-argument (first args))
    (doseq [arg (rest args)]
      (print " ")
      (write-argument arg))
    (newline)))

(declare group-sizes stamp<?)

(defn- done
  "Print out final report."
  [mesg ntest stamps]
  (print mesg)
  (print " ")
  (print ntest)
  (print " tests")
  (let [sorted (sort stamp<? (filter #(and (seq? %) (seq %)) stamps))
        grouped (group-sizes sorted)
        entries (map (fn [p]
                       (let [n (first p)
                             lis (rest p)]
                         (str (quot (* 100 n) ntest)
                           "% "
                           (clojure.string/join ", " lis))))
                  (sort (fn [p1 p2]
                          (> (first p1) (first p2)))
                    grouped))]
    (cond
      (not (seq entries)) (println ".")
      (not (seq (rest entries))) (do
                                   (print " (")
                                   (print (first entries))
                                   (println ")."))
      :else
      (do
        (println ".")
        (doseq [entry entries]
          (print entry)
          (println "."))))))


(defn- group-sizes
  "Compute class-group sizes."
  [lis]
  (if (not (seq lis))
    []
    (loop [current (first lis)
           size 1
           lis (rest lis)
           res []]
      (cond
        (not (seq lis)) (reverse (conj res (list size current)))
        (= current (first lis)) (recur current (+ 1 size) (rest lis) res)
        :else
        (recur (first lis) 1 (rest lis) (conj res (list size current)))))))

(defn- stamp<?
  "Compare two stamps."
  [s1 s2]
  (cond
    (not (seq s1)) (seq s2)
    (not (seq s2)) true
    :else
    (let [c (compare (first s1) (first s2))]
      (cond
        (< c 0) true
        (= c 0) (stamp<? (rest s1) (rest s2))
        :else false))))

(defn ^:no-doc quickcheck-report [msg prop-sexpr result]
  (let [[ntests stamps success] result]
    (case success
      true (do-report {:type :pass, :message msg,
                       :expected prop-sexpr})
      false (do-report {:type :fail, 
                        :message (str "Arguments exhausted after " ntests " tries"),
                        :expected prop-sexpr, :actual false})
      (do-report {:type :fail,
                  :message (str "falsifiable")
                  :expected prop-sexpr
                  :actual (check-result-arguments-list success)}))))

(defmethod assert-expr 'quickchecked [msg form]
  ;; (is (quickchecked prop))
  ;; Asserts that the property passes the QuickCheck tests
  (let [prop (second form)]
    `(quickcheck-report ~msg '~prop (quickcheck-results ~prop))))

(defmethod assert-expr 'quickcheck [msg form]
  ;; deprecated alias of quickchecked.
  (assert-expr msg (apply list 'quickchecked (rest form))))

;; --- Distribution tools ---------

(let [incc (fn [i]
             (if i (inc i) 1))]

  (defn ^:no-doc occurrences [stamps]
    (reduce (fn [acc labels]
              (reduce (fn [acc label]
                        (update acc label incc)) acc labels))
            {} stamps)))

(let [map-values (fn [f m]
                   (into {} (for [[k v] m] [k (f v)])))]

  (defn ^:no-doc distribution [stamps]
    (let [n (count stamps)]
      (map-values (fn [x] (/ x n))
                  (occurrences stamps)))))

(defn ^:no-doc fraction-of [label stamps]
  (let [d (distribution stamps)]
    (or (get d label) 0)))

(defn ^:no-doc distributed?
  "Check stamp distribution"
  [stamps required-distribution]
  (let [actual-distribution (distribution stamps)]
    (every? (fn [[label lower-bound]]
              (>= (get actual-distribution label) lower-bound))
            required-distribution)))

(defn ^:no-doc with-distribution-report [msg prop-sexpr pairs result]
  (let [[ntests stamps success] result
        d? (distributed? stamps pairs)]
    (if (and success (not d?))
      (do-report {:type :fail, :message "Distribution requirements not met",
                  :expected pairs
                  :actual (distribution stamps)})
      (quickcheck-report msg prop-sexpr result))))

(defmethod assert-expr 'with-distribution [msg form]
  ;; (is (with-distribution [label fraction ...] (quickcheck prop)))
  ;; Asserts that the property passes the QuickCheck tests
  ;; and meets the required label distribution
  (let [pairs (drop-last (drop 1 form))
        qcform (last form)]
    (let [prop (second qcform)]
      `(with-distribution-report ~msg '~prop (hash-map ~@pairs) (quickcheck-results ~prop)))))
