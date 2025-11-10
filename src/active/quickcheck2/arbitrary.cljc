(ns active.quickcheck2.arbitrary
  (:require #?(:clj [active.data.record :refer [def-record is-a?]]
               :cljs [active.data.record :refer [is-a?] :refer-macros [def-record]]))
  (:require [active.quickcheck2.generator :as generator])
  (:require [active.clojure.monad :as monad])
  (:require [active.quickcheck2.generator-applicative :refer [with-tree
                                                              combine-generators]])
  (:require [clojure.math.numeric-tower :refer [expt]])
  )

(def-record ^{:doc "Generalization of generator, suitable for producing function generators."}
  Arbitrary-type
  [arbitrary-generator])

(defn make-arbitrary
  [generator]
  (Arbitrary-type arbitrary-generator generator))

(def-record ^{:doc "Coarbitrary typeclass in original Haskell implementation"}
  Coarbitrary-type
  [coarbitrary-coarbitrary])

(defn make-coarbitrary
  [coarb]
  (Coarbitrary-type coarbitrary-coarbitrary coarb))

(defn such-that
  "Takes a arbitary and a predicate and
  returns a new generator that satisfies
  the predicate."
  [arb pred]
  (let [gen (arbitrary-generator arb)
        newgen (generator/such-that-generator gen pred)]
    (make-arbitrary newgen))) ;; TODO: write coarbitrary implementation

(defn generate-one-of
  "Randomly choose one of a list of given arbitraries"
  [arbs]
  (monad/free-bind (with-tree (generator/choose-one-of arbs))
                   arbitrary-generator))

;; Arbitraries
;; -----------

(def arbitrary-boolean
  "Arbitrary boolean."
  (make-arbitrary
    (generator/choose-one-of '(true false))))

(def coarbitrary-boolean
  "Coarbitrary boolean"
  (make-coarbitrary
   (fn [a gen]
     (generator/variant (if a 0 1) gen))))

(def arbitrary-integer
  "Arbitrary integer."
  (make-arbitrary
   (generator/sized
    (fn [n]
      (let [hi (expt 4 n)
            lo (- hi)]
        (generator/choose-integer lo hi))))))

(def coarbitrary-integer
  "Arbitrary integer."
  (make-coarbitrary
    (fn [n gen]
      (generator/variant (if (>= n 0)
                           (* 2 n)
                           (+ (* 2 (- n)) 1))
        gen))))

(def arbitrary-natural
  "Arbitrary natural number."
  (make-arbitrary
   (generator/sized
    (fn [n]
      (generator/choose-integer 0 n)))))

(def coarbitrary-natural
  "Coarbitrary natural number"
  (fn [n gen]
    (generator/variant n gen)))

(defn arbitrary-integer-from-to
  "Arbitrary integer from range."
  [from to]
  (make-arbitrary
   (generator/sized
    (fn [n]
      (generator/choose-integer from to)))))

(defn coarbitrary-integer-from-to
  "Coarbitrary integer from range."
  [from to]
  (fn [n gen]
      (generator/variant (- n from) gen)))

; TODO can we remove this
(defn- arbitrary-int-like
  [gen to-int]
  (make-arbitrary 
   gen))

(defn- coarbitrary-int-like
  [gen to-int]
  (make-coarbitrary (fn [v rgen]
                      (generator/variant (to-int v) rgen))))

(def arbitrary-byte
  "Arbitrary byte."
  (arbitrary-int-like generator/choose-byte byte))

(def coarbitrary-byte
  "Coarbitrary byte."
  (coarbitrary-int-like generator/choose-byte byte))

(def arbitrary-short
  "Arbitrary short."
  (arbitrary-int-like generator/choose-short short))

(def coarbitrary-short
  "Coarbitrary short."
  (coarbitrary-int-like generator/choose-short short))

(def arbitrary-int
  "Arbitrary int."
  (arbitrary-int-like generator/choose-int int))

(def coarbitrary-int
  "Coarbitrary int."
  (coarbitrary-int-like generator/choose-int int))

(def arbitrary-long
  "Arbitrary long."
  (arbitrary-int-like generator/choose-long long))

(def coarbitrary-long
  "Coarbitrary long."
  (coarbitrary-int-like generator/choose-long long))

(def arbitrary-unsigned-byte
  "Arbitrary unsigned byte."
  (arbitrary-int-like generator/choose-unsigned-byte short))

(def coarbitrary-unsigned-byte
  "Coarbitrary unsigned byte."
  (coarbitrary-int-like generator/choose-unsigned-byte short))

(def arbitrary-unsigned-short
  "Arbitrary unsigned short."
  (arbitrary-int-like generator/choose-unsigned-short int))

(def coarbitrary-unsigned-short
  "Coarbitrary unsigned short."
  (coarbitrary-int-like generator/choose-unsigned-short int))

(def arbitrary-unsigned-int
  "Arbitrary unsigned int."
  (arbitrary-int-like generator/choose-unsigned-int long))

(def coarbitrary-unsigned-int
  "Coarbitrary unsigned int."
  (coarbitrary-int-like generator/choose-unsigned-int long))

(def arbitrary-unsigned-long
  "Arbitrary unsigned long."
  (arbitrary-int-like generator/choose-unsigned-long bigint))

(def coarbitrary-unsigned-long
  "Coarbitrary unsigned long."
  (coarbitrary-int-like generator/choose-unsigned-long bigint))

(def arbitrary-ascii-char
  "Arbitrary ASCII character."
  (arbitrary-int-like generator/choose-ascii-char int))

(def coarbitrary-ascii-char
  "Coarbitrary ASCII character."
  (coarbitrary-int-like generator/choose-ascii-char int))

(def arbitrary-ascii-letter
  "Arbitrary ASCII letter."
  (arbitrary-int-like generator/choose-ascii-letter int))

(def coarbitrary-ascii-letter
  "Coarbitrary ASCII letter."
  (coarbitrary-int-like generator/choose-ascii-letter int))

(def arbitrary-printable-ascii-char
  "Arbitrary printable ASCII character."
  (arbitrary-int-like generator/choose-printable-ascii-char int))

(def coarbitrary-printable-ascii-char
  "Coarbitrary printable ASCII character."
  (coarbitrary-int-like generator/choose-printable-ascii-char int))

(def arbitrary-char
  "Arbitrary char."
  (arbitrary-int-like (generator/sized
                       (fn [n]
                         (generator/choose-char \u0000 (char (min n 0xffff)))))
                      int))

(def coarbitrary-char
  "Coarbitrary char."
  (coarbitrary-int-like (generator/sized
                         (fn [n]
                           (generator/choose-char \u0000 (char (min n 0xffff)))))
                        int))

(defn- make-rational
  [a b]
  (/ a
    (+ 1 b)))

(def arbitrary-rational
  "Arbitrary rational number."
  (make-arbitrary
    (combine-generators make-rational
      (arbitrary-generator arbitrary-integer)
      (arbitrary-generator arbitrary-natural))))

(def coarbitrary-rational
  "Coarbitrary rational number."
  (make-coarbitrary
   (fn [^clojure.lang.Ratio r gen]
     ((coarbitrary-coarbitrary coarbitrary-integer)
      (.numerator r)
      ((coarbitrary-coarbitrary coarbitrary-integer)
       (.denominator r) gen)))))

(defn- fraction
  [a b c]
  (+ a
    (float (/ b
             (+ (abs c) 1)))))

(def arbitrary-float
  "Arbitrary float."
  (make-arbitrary
   (combine-generators fraction
                       (arbitrary-generator arbitrary-integer)
                       (arbitrary-generator arbitrary-integer)
                       (arbitrary-generator arbitrary-integer))))

(def coarbitrary-float
  "Coarbitrary float."
  (make-coarbitrary
   (fn [r gen]
     (let [^clojure.lang.Ratio fr (rationalize r)]
       ((coarbitrary-coarbitrary coarbitrary-integer)
        (.numerator fr)
        ((coarbitrary-coarbitrary coarbitrary-integer)
         (.denominator fr) gen))))))

(defn arbitrary-mixed
  "Arbitrary value from one of a list of (promises of) arbitraries."
  [pred+arbitrary-promise-list]
  (make-arbitrary
   (generator/choose-mixed (map #(delay (arbitrary-generator (force (second %))))
                                pred+arbitrary-promise-list))))

(defn coarbitrary-mixed
  "Arbitrary value from one of a list of (promises of) arbitraries."
  [pred+arbitrary-promise-list]
  (make-coarbitrary
    (fn [val gen]
      (loop [lis pred+arbitrary-promise-list
             n 0]
        (cond
          (not (seq lis)) (throw (Error. "arbitrary-mixed: value matches none of the predicates"))
          ((first (first lis)) val) (generator/variant n gen)
          :else (recur (rest lis) (+ 1 n)))))))

(defn arbitrary-one-of
  "Arbitrary value from a list of values, and equality predicate."
  [eql? & vals]
  (make-arbitrary
   (generator/choose-one-of vals)))

(defn coarbitrary-one-of
  "Coarbitrary value from a list of values, and equality predicate."
  [eql? & vals]
  (make-coarbitrary
    (fn [val gen]
      (loop [lis vals
             n 0]
        (cond
          (not (seq lis)) (throw (Error. "arbitrary-one-of: value matches none of the predicates"))
          (eql? (first lis) val) (generator/variant n gen)
          :else (recur (rest lis) (+ 1 n)))))))

(defn arbitrary-tuple
  "Arbitrary fixed-size vector."
  [& arbitrary-els]
  (make-arbitrary
    (apply combine-generators
      vector
      (map arbitrary-generator arbitrary-els))))

(defn coarbitrary-tuple
  [& coarbitrary-els]
  (make-coarbitrary
   (fn [lis gen]
     (letfn [(recurse [coarbitrary-els lis]
               (if (seq coarbitrary-els)
                 ((coarbitrary-coarbitrary (first coarbitrary-els))
                  (first lis)
                  (recurse (rest coarbitrary-els)
                           (rest lis)))
                 gen))]
       (recurse coarbitrary-els lis)))))

(defn arbitrary-record
  "Arbitrary record."
  [construct accessors & arbitrary-els]
  (make-arbitrary
   (apply combine-generators
          construct
          (map arbitrary-generator arbitrary-els))))

(defn coarbitrary-record
  "Coarbitrary record."
  [construct accessors & coarbitrary-els]
  (make-coarbitrary
    (fn [rec gen]
      (letfn [(recurse [coarbitrary-els lis]
                (if (seq coarbitrary-els)
                  ((coarbitrary-coarbitrary (first coarbitrary-els))
                    (first lis)
                    (recurse (rest coarbitrary-els) (rest lis)))
                  gen))]
        (recurse coarbitrary-els
          (map #(% rec) accessors))))))

(defn arbitrary-coll-of
  "Arbitrary collection mimicking Clojure spec's coll-of"
  [arbitrary-el & kwargs]
  (let [opts (apply hash-map kwargs)
        {kind :kind, :or {kind 'clojure.core/vector?}} opts
        list->sequence (cond
                         (= kind 'clojure.core/vector?) vec
                         (= kind 'clojure.core/list?) #(into () %)
                         (= kind 'clojure.core/set?) set)
        {count :count} opts
        {min-count :min-count, :or {min-count 0}} opts
        {max-count :max-count} opts
        generator-el (arbitrary-generator arbitrary-el)]
    (make-arbitrary
     (generator/sized
      (fn [n]
        (combine-generators
         list->sequence
        (if count
          (generator/choose-list generator-el count)
          (generator/choose-sequence-like-in-range generator-el
                                                   min-count
                                                   (if max-count max-count n)))))))))

(defn coarbitrary-coll-of
  "Coarbitrary collection mimicking Clojure spec's coll-of"
  [arbitrary-el & kwargs]
  :not-supported-yet)

(defn arbitrary-sequence-like
  "Arbitrary sequence-like container."
  [list->sequence arbitrary-el]
  (make-arbitrary
    (generator/sized
      (fn [n]
        (combine-generators list->sequence
                            (generator/choose-sequence-like-in-range (arbitrary-generator arbitrary-el) 0 n))))))

(defn arbitrary-sequence-like-in-range
  "Arbitrary sequence-like container."
  [list->sequence arbitrary-el lower upper]
  (make-arbitrary
   (generator/sized
    (fn [n]
      (generator/choose-sequence-like-in-range (arbitrary-generator arbitrary-el)
                                               lower
                                               (min (+ lower n) upper))))))

(defn coarbitrary-sequence-like
  "Coarbitrary sequence-like container."
  [choose-sequence sequence->list coarbitrary-el]
  (make-coarbitrary
    (fn [sequ gen]
      (letfn [(recurse [lis]
                (if (seq lis)
                  ((coarbitrary-coarbitrary coarbitrary-el)
                    (first lis)
                    (generator/variant 1 (recurse (rest lis))))
                  (generator/variant 0 gen)))]
        (recurse (sequence->list sequ))))))

(defn arbitrary-list
  "Arbitrary list."
  [arbitrary-el]
  (arbitrary-sequence-like #(into () %) arbitrary-el))

(defn arbitrary-list-in-range
   "Arbitrary list in range (lower,uppper)."
   [arbitrary-el lower upper]
   (arbitrary-sequence-like-in-range #(into () %) arbitrary-el lower upper))

(defn coarbitrary-list
  "Coarbitrary list."
  [coarbitrary-el]
  (coarbitrary-sequence-like generator/choose-list identity coarbitrary-el))

(defn arbitrary-vector
  "Arbitrary vector."
  [arbitrary-el]
  (arbitrary-sequence-like vec arbitrary-el))

(defn arbitrary-vector-in-range
  "Arbitrary vector in range (lower,uppper)."
  [arbitrary-el lower upper]
  (arbitrary-sequence-like-in-range vec arbitrary-el lower upper))

(defn coarbitrary-vector
  "Coarbitrary vector."
  [coarbitrary-el]
  (coarbitrary-sequence-like generator/choose-vector #(into () %) coarbitrary-el))

(def arbitrary-byte-array
  "Arbitrary byte-array."
  (arbitrary-sequence-like byte-array arbitrary-byte))

(defn arbitrary-byte-array-in-range
  "Arbitrary byte-array in range (lower,uppper)."
  [arbitrary-el lower upper]
  (arbitrary-sequence-like-in-range byte-array arbitrary-el lower upper))

(def coarbitrary-byte-array
  "coarbitrary byte-array."
  (coarbitrary-sequence-like (fn [_ n] (generator/choose-byte-array n)) #(into () %) coarbitrary-byte))

(defn arbitrary-map
  "Arbitrary map over the given arbitrary key and value."
  [arbitrary-key arbitrary-value]
  (arbitrary-sequence-like generator/map-of-tuples (arbitrary-tuple arbitrary-key arbitrary-value)))

(defn coarbitrary-map
  "coarbitrary map over the given arbitrary key and value."
  [coarbitrary-key coarbitrary-value]
  (coarbitrary-sequence-like generator/choose-map #(into () %) (coarbitrary-tuple coarbitrary-key coarbitrary-value)))

(defn arbitrary-set
  "Arbitrary set."
  [arbitrary-el]
  (arbitrary-sequence-like set arbitrary-el))

(defn coarbitrary-set
  "Coarbitrary set."
  [coarbitrary-el]
  (coarbitrary-sequence-like generator/choose-set #(into () %) coarbitrary-el))

(def arbitrary-ascii-string
  "Arbitrary string of ASCII characters."
  (arbitrary-sequence-like #(apply str %) arbitrary-ascii-char))

(defn arbitrary-ascii-string-in-range
  "Arbitrary string of ASCII characters in range (lower,upper)."
  [lower upper]
  (arbitrary-sequence-like-in-range #(apply str %) arbitrary-ascii-char lower upper))

(def coarbitrary-ascii-string
  "Coarbitrary string of ASCII characters."
  (coarbitrary-sequence-like #(apply str %) #(into () %) coarbitrary-ascii-char))

(def arbitrary-printable-ascii-string
  "Arbitrary string of printable ASCII characters."
  (arbitrary-sequence-like #(apply str %) arbitrary-printable-ascii-char))

(defn arbitrary-printable-ascii-string-in-range
  "Arbitrary string of printable ASCII characters in range (lower,upper)."
  [lower upper]
  (arbitrary-sequence-like-in-range #(apply str %) arbitrary-printable-ascii-char lower upper))

(def arbitrary-string
  "Arbitrary string."
  (arbitrary-sequence-like #(apply str %) arbitrary-char))

(defn arbitrary-string-in-range
  "Arbitrary string in range (lower,upper)."
  [lower upper]
  (arbitrary-sequence-like-in-range #(apply str %) arbitrary-char lower upper))

(def coarbitrary-string
  "Coarbitrary string."
  (coarbitrary-sequence-like #(apply str %) #(into () %) coarbitrary-char))

(defn- arbitrary-symbol-like
  [choose]
  (make-arbitrary
   (generator/sized (fn [n] (choose n)))))

(defn- coarbitrary-symbol-like
  [choose]
  (make-coarbitrary
    (fn [v gen]
      ((coarbitrary-coarbitrary coarbitrary-string) (name v) gen))))

(def arbitrary-symbol
  "Arbitrary symbol."
  (arbitrary-symbol-like generator/choose-symbol))

(def coarbitrary-symbol
  "Coarbitrary symbol."
  (coarbitrary-symbol-like generator/choose-symbol))

(def arbitrary-keyword
  "Arbitrary keyword."
  (arbitrary-symbol-like generator/choose-keyword))

(def coarbitrary-keyword
  "Coarbitrary keyword."
  (coarbitrary-symbol-like generator/choose-keyword))

(defn arbitrary-function
  "Arbitrary function."
  [arbitrary-result & coarbitrary-args]
  (let [coarbitrary-arg-tuple (apply coarbitrary-tuple coarbitrary-args)]
    (make-arbitrary
     (generator/promote
      (fn [& args]
        ((coarbitrary-coarbitrary coarbitrary-arg-tuple)
         args
         (arbitrary-generator arbitrary-result)))))))

(defn coarbitrary-function
  "Coarbitrary function."
  [coarbitrary-result & arbitrary-args]
  (let [arbitrary-arg-tuple (apply arbitrary-tuple arbitrary-args)]
    (make-coarbitrary
     (fn [func gen]
       (monad/monadic
        [args (arbitrary-generator arbitrary-arg-tuple)
         t
         ((coarbitrary-coarbitrary coarbitrary-result)
          (apply func args)
          gen)]
        (monad/return t))))))

(def arbitrary-any
  "Arbitrary for values of any type."
  (arbitrary-mixed [[nil? (arbitrary-one-of = nil)]
                    [keyword? arbitrary-keyword]
                    [string? arbitrary-string]
                    [int? arbitrary-int]
                    [integer? arbitrary-integer]
                    [boolean? arbitrary-boolean]
                    [symbol? arbitrary-symbol]
                    ;; TODO: Arbitraries that expect an element type (such as
                    ;; set, map, ...).
                    ]))

(defn symbol->arbitrary
  [sym]
  (cond
    (= sym `integer?) arbitrary-integer
    (= sym `string?) arbitrary-string
    (= sym `keyword?) arbitrary-keyword
    (= sym `any?) arbitrary-any))

(defn symbol->coarbitrary
  [sym]
  (cond
    (= sym `integer?) coarbitrary-integer
    (= sym `string?) coarbitrary-string
    (= sym `keyword?) coarbitrary-keyword))

(defn fn->arbitrary
  [fun]
  (cond
    (= fun integer?) arbitrary-integer
    (= fun string?) arbitrary-string
    (= fun keyword?) arbitrary-keyword))

(defn fn->coarbitrary
  [fun]
  (cond
    (= fun integer?) coarbitrary-integer
    (= fun string?) coarbitrary-string
    (= fun keyword?) coarbitrary-keyword))

(defn set->arbitrary
  "Make an arbitrary from a set (behaviour like enum)"
  [s]
  (apply arbitrary-one-of (into [identity] s)))

(defn set->coarbitrary
  "Make a coarbitrary from a set (behaviour like enum)"
  [s]
  (apply coarbitrary-one-of (into [identity] s)))
