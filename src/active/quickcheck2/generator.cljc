(ns active.quickcheck2.generator
  (:require [active.quickcheck2.random
             :refer [random-integer random-float random-generator-split integer-variant]])
  (:require #?(:clj [active.data.record :refer [def-record is-a?]]
               :cljs [active.data.record :refer [is-a?] :refer-macros [def-record]]))
  (:require [active.quickcheck2.tree :as tree])
  (:require [active.clojure.monad :as monad])
  (:require [active.quickcheck2.generator-applicative :refer [with-tree?
                                                              with-tree
                                                              get-tree
                                                              integrated
                                                              combine-generators
                                                              combine-generators-curry]])
  (:require [active.quickcheck2.shrink :as shrink])
  (:require [clojure.math.numeric-tower :refer [expt]]))

(def lift->generator combine-generators)

(def-record ^{:doc "Get the random generator."}
  Get-random-generator-type [])
(def get-random-generator (Get-random-generator-type))
(defn get-random-generator? [x] (is-a? Get-random-generator-type x))

(def-record ^{:doc "Get the size of the random generator."}
  Get-size-type [])
(def get-size (Get-size-type))
(defn get-size? [x] (is-a? Get-size-type x))

(defn sequ-with-tree
  "Evaluate each action in the sequence from left to right, and collect the results."
  [ms]
  (let [f (fn f [ms res]
            (if (seq ms)
              (monad/free-bind (with-tree (first ms))
                         (fn [v]
                           (f (rest ms)
                              (cons v res))))
              (monad/free-return (reverse res))))]
    (f ms '())))

;; Basic generator combinators
;; ---------------------------

; TODO change all (tree/make-Tree n []) with proper shrink trees
; [lower, upper]
(defn choose-integer
  "Generator for integers within a range, bounds are inclusive."
  [lower upper]
  (monad/monadic
    [rgen get-random-generator]
    (let [[n _] (random-integer rgen lower upper)
          ;shrinkTowards the nearest number to zero within the range
          towards-num (cond
                        (neg? upper) upper
                        (neg? lower) 0
                        :else lower)])
    (monad/return (tree/unfold (partial shrink/shrink-towards towards-num) n))))

(def choose-byte
  "Generator for bytes in [-128, 127]."
  (combine-generators
   byte
   (choose-integer Byte/MIN_VALUE Byte/MAX_VALUE)))
  
(def choose-unsigned-byte
  "Generator for bytes in [0, 255]."
  (combine-generators
   short
   (choose-integer 0 (- (expt 2 Byte/SIZE) 1))))
  
(def choose-short
  "Generator for shorts in [-32768, 32767]."
  (combine-generators
   short
   (choose-integer Short/MIN_VALUE Short/MAX_VALUE)))

(def choose-unsigned-short
  "Generator for bytes in [0, 65535]."
  (combine-generators
   int
   (choose-integer 0 (- (expt 2 Short/SIZE) 1))))

(def choose-int
  "Generator for ints in [-2147483648, 2147483647]."
  (combine-generators
   int
   (choose-integer Integer/MIN_VALUE Integer/MAX_VALUE)))

(def choose-unsigned-int
  "Generator for bytes in [0, 4294967295]."
  (combine-generators
   long
   (choose-integer 0 (- (expt 2 Integer/SIZE) 1))))

(def choose-long
  "Generator for longs in [-9223372036854775808, 9223372036854775807]."
  (combine-generators
   long
   (choose-integer Long/MIN_VALUE Long/MAX_VALUE)))

(def choose-unsigned-long
  "Generator for bytes in [0, 18446744073709551615]."
  (combine-generators
   bigint
   (choose-integer 0 (- (expt 2 Long/SIZE) 1))))

(defn choose-float
  "Generator for floats within a range, bounds are inclusive."
  [lower upper]
  (monad/monadic
    [rgen get-random-generator]
    (let [[n _] (random-float rgen lower upper)])
    (monad/return (tree/tree tree/tree-outcome n tree/tree-shrinks []))))

(def choose-ascii-char
  "Generator for ASCII characters."
  (combine-generators char (choose-integer 0 127)))

(def choose-ascii-letter
  "Generator for ASCII alphabetic letters."
  (combine-generators #(get "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" %)
                      (choose-integer 0 51)))

(def choose-printable-ascii-char
  "Generator for printable ASCII characters."
  (combine-generators char (choose-integer 32 127)))

(defn- choose-char-with-property
  [pred]
  (monad/monadic
    [rgen get-random-generator]
;   ;; loop until proper char is found; otherwise we could build a
;   ;; map of all chars, but that's not that good either.
    (loop [rg rgen]
          (let [[i ngen] (random-integer rg 0 0xffff)
                c (char i)]
            (if (pred c)
             (monad/return (tree/tree tree/tree-outcome c tree/tree-shrinks []))
             (recur ngen))))))

(def choose-non-numeric-char
  (letfn [(is-non-numeric? [c]
            (let [t (java.lang.Character/getType (int c))]
              (or (= t java.lang.Character/UPPERCASE_LETTER)
                  (= t java.lang.Character/LOWERCASE_LETTER))))]
    (choose-char-with-property is-non-numeric?)))

(def choose-alphanumeric-char
  (letfn [(is-alphanumeric? [c]
            (let [t (java.lang.Character/getType (int c))]
              (or (= t java.lang.Character/UPPERCASE_LETTER)
                  (= t java.lang.Character/LOWERCASE_LETTER)
                  (= t java.lang.Character/DECIMAL_DIGIT_NUMBER)
                  (= t java.lang.Character/LETTER_NUMBER)
                  (= t java.lang.Character/OTHER_NUMBER)))
            )]
    (choose-char-with-property is-alphanumeric?)))

(defn choose-char
  "Generator for chars within a range, bonds are inclusive."
  [lower upper]
  (combine-generators char (choose-integer (int lower) (int upper))))

; int (generator a) -> (generator a)
(def-record ^{:doc "Make generator that transforms random number seed depending on v."}
  Variant-type
  [variant-v
    variant-generator])
(defn variant [v generator] (Variant-type variant-v v variant-generator generator))
(defn variant? [x] (is-a? Variant-type x))

; (vals -> (generator b)) -> (generator (vals -> b))
(def-record ^{:doc "Promote a function to generators to a generator of functions."}
  Promote-type
  [promote-func])
(defn promote [func] (Promote-type promote-func func))
(defn promote? [x] (is-a? Promote-type x))

(defrecord MyFn [f graph]
  clojure.lang.IFn
  (invoke [this]
    (let [ret (f)]
      (swap! graph assoc [] ret)
      ret))
  (invoke [this arg]
    (let [ret (f arg)]
      (swap! graph assoc [arg] ret)
      ret))
  (invoke [this arg1 arg2]
    (let [ret (f arg1 arg2)]
      (swap! graph assoc [arg1 arg2] ret)
      ret))
  (invoke [this arg1 arg2 arg3]
    (let [ret (f arg1 arg2 arg3)]
      (swap! graph assoc [arg1 arg2 arg3] ret)
      ret))
  (invoke [this arg1 arg2 arg3 arg4]
    (let [ret (f arg1 arg2 arg3 arg4)]
      (swap! graph assoc [arg1 arg2 arg3 arg4] ret)
      ret))
  (invoke [this arg1 arg2 arg3 arg4 arg5]
    (let [ret (f arg1 arg2 arg3 arg4 arg5)]
      (swap! graph assoc [arg1 arg2 arg3 arg4 arg5] ret)
      ret))
  (applyTo [this args]
    (let [ret (apply f args)]
      (swap! graph assoc args ret)
      ret))
  (invoke [this arg1 arg2 arg3 arg4 arg5 & args]
    (let [ret (apply f arg1 arg2 arg3 arg4 arg5 & args)]
      (swap! graph assoc (apply conj [] arg1 arg2 arg3 arg4 arg5 args) ret)
      ret))
  Object
  (toString [this] (str "Function: " (deref (:graph this)))))

(defn current-graph [myfn]
  (deref (:graph myfn)))

(defn function-memorize? [arg] (instance? MyFn arg))

(defmacro fm
  [[ & args] [ & body]]
  (let [f# `(fn ~(vec args) ~body)
        graph# `(atom {})]
    `(->MyFn ~f# ~graph# )))

(def-record ^{:doc "Make a generator with a specified size."}
  With-size-type
  [with-size-size
   with-size-generator])
(defn resize [size generator] (With-size-type with-size-size size with-size-generator generator))
(defn with-size? [x] (is-a? With-size-type x))
; int random-gen (generator a) -> a

(def-record ^{:doc "If the generator contains a tree, use it"}
  Maybe-with-tree-type
  [maybe-get-tree])
(defn maybe-with-tree [get-tree] (Maybe-with-tree-type maybe-get-tree get-tree))
(defn maybe-with-tree? [x] (is-a? Maybe-with-tree-type x))

(def-record ^{:doc "Get the maximal depth for the shrinking function"}
  Get-max-shrink-depth-type [])
(defn get-max-shrink-depth [] (Get-max-shrink-depth-type))
(defn get-max-shrink-depth? [x] (is-a? Get-max-shrink-depth-type x))

(defn coerce->tree [arg] (if (tree/tree? arg) (tree/tree-outcome arg) arg))

(defn value->tree [arg] (if (tree/tree? arg) arg (tree/pure arg)))

(defn generate ; aka run
  "Extract a value from a generator, using size n and random generator rgen."
  [n rgen gen max-shrink-depth]
  (let [[size nrgen] (random-integer rgen 0 n)]
    (letfn [(run [m size rgen]
      (cond
        (monad/free-return? m) (monad/free-return-val m)
                
        (monad/free-bind? m)
        (let [m1 (monad/free-bind-monad m)
              cont (monad/free-bind-cont m)
              [rgen1 rgen2] (random-generator-split rgen)]
          (cond
            (with-tree? m1) (recur (cont (value->tree (run (get-tree m1) size rgen1))) size rgen2)

            (maybe-with-tree? m1) (recur (cont (run (maybe-get-tree m1) size rgen1)) size rgen2)

            (monad/free-return? m1) (recur (cont (coerce->tree (monad/free-return-val m1))) size rgen)
            
            (monad/free-bind? m1) (ex-info "nested bind; should not happen" {:fn `run :expression m :bind-first m1})
            
            (get-random-generator? m1) (recur (cont rgen2) size rgen1)
            
            (get-size? m1) (recur (cont size) size rgen)

            (get-max-shrink-depth? m1) (recur (cont max-shrink-depth) size rgen)
           
            (with-size? m1)
            (let [size1 (with-size-size m1)
                  gen1 (with-size-generator m1)]
              (recur (cont (run gen1 size1 rgen1)) size rgen2))
            
            (variant? m1)
            (let [v (variant-v m1)
                  next-rgen (integer-variant v rgen)]
              (run (cont (run (variant-generator m1) size next-rgen)) size next-rgen))
            
            (promote? m1)
            (recur
              (cont
                (let [func (promote-func m1)]
                  (tree/lazy-tree
                  (fm [& vals]
                    (let [b (run (apply func vals) size rgen1)]
                       (tree/tree-outcome b))) [])))
              size rgen2)
            :else (assert false
                          (str "invalid generator: " (pr-str m1)))))
                
        (get-random-generator? m) rgen
                
        (get-size? m) size
        
        (get-max-shrink-depth? m) max-shrink-depth

        (with-size? m)
        (let [size (with-size-size m)
              gen (with-size-generator m)]
          (recur gen size rgen))
                
        (variant? m)
        (let [v (variant-v m)
              next-rgen (integer-variant v rgen)]
          (run (variant-generator m) size next-rgen))
                
        (promote? m)
        (let [func (promote-func m)]
          (tree/lazy-tree
          (fm [& vals]
             (let [b (run (apply func vals) size rgen)]
               (tree/tree-outcome b)))
          []))
        
      :else (assert false
                    (str "invalid gen: " (pr-str m)))))]
      (run gen size nrgen))))

; (int -> (generator a)) -> (generator a)
(defn sized
  "Apply a size to a generator."
  [func]
  (monad/monadic
    [size get-size]
    (func size)))

; (list a) -> (generator a)
; TODO does it make sense to shrink this like an integer?
(defn choose-one-of
  "Make a generator that yields one of a list of values."
  [lis]
  (combine-generators #(nth lis %)
    (choose-integer 0 (- (count lis) 1))))

; (list (gen a)) -> (gen a)
; TODO doesn't work with trees. Remove it?
(defn oneof
  "Haskell QuickCheck's oneof"
  [gs]
  (when (< (count gs) 1)
    (assert false "oneof used with empty list"))
  (monad/free-bind (choose-integer 0 (- (count gs) 1))
                   #(nth gs 1)))

(defn choose-sequence-like-in-range
  [el-gen lower upper]
  (monad/monadic
   [length (choose-integer lower upper)]
   [list-of-trees (sequ-with-tree (repeat length el-gen))]
   (monad/return (first (tree/filter-tree (fn [sequence] (<= lower (count sequence)))
                                          (shrink/sequence-shrink-list list-of-trees))))))

; vector from the paper
; (generator a) int -> (generator (list a))
(defn choose-list
  "Generator for a list of values with size n."
  [el-gen n]
  (apply combine-generators list (repeat n el-gen)))

(defn choose-list-in-range
  [el-gen lower upper]
  (combine-generators #(into () %) (choose-sequence-like-in-range lower upper)))

; (generator char) int -> (generator string)
(defn choose-string
  "Generator for a string with size n."
  [char-gen n]
  (combine-generators #(apply str %) (choose-list char-gen n)))

(defn choose-string-in-range
  [el-gen lower upper]
  (combine-generators #(apply str %) (choose-sequence-like-in-range lower upper)))

(declare choose-mixed)
; TODO make it work with trees
(defn choose-symbol
  "Generator for a symbol with size n+1."
  [n]
  (let
    [fst (choose-string choose-non-numeric-char 1)
     rst (choose-string (choose-mixed (list choose-alphanumeric-char
                                        (choose-one-of (seq "*+!-_?"))))
           n)]
    (combine-generators-curry (fn [f] (fn [r] (symbol (str f r)))) fst rst)))

(defn choose-keyword
  "Generator for a keyword with size n+1."
  [n]
  (combine-generators keyword (choose-symbol n)))

(defn choose-vector
  "Generator for a vector with size n."
  [el-gen n]
  (combine-generators vec (choose-list el-gen n)))

(defn choose-vector-in-range
  [el-gen lower upper]
  (combine-generators vec (choose-sequence-like-in-range el-gen lower upper)))

(defn choose-byte-array
  "Generator for a byte array with size n."
  [n]
  (combine-generators byte-array (choose-list choose-byte n)))

(defn choose-byte-array-in-range
  [el-gen lower upper]
  (combine-generators byte-array (choose-sequence-like-in-range el-gen lower upper)))

(defn choose-map
  "Generator for a map with size n. The passed element generator must
  generate key-value pairs."
  [el-gen n]
  ;; TODO: should this generate unique keys, maybe?
  (combine-generators (partial into {}) (choose-list el-gen n)))

(defn choose-set
  "Generator for a set with size <= n"
  [el-gen n]
  (combine-generators set (choose-list el-gen n)))

; (list (promise (generator a))) -> (generator a)
(defn choose-mixed
  "Generator that chooses from a sequence of generators.
  This has no shrinking between the gens"
  [gens]
  (monad/monadic
   [n (choose-integer 0 (- (count gens) 1))]
   (force (nth gens n))))
  ;(monad/free-bind (choose-one-of gens) force)) ; ???

; (list (list int (generator a))) -> (generator a)
(declare pick)
(defn choose-with-frequencies
  "Generator that chooses from a sequence of (frequency generator) pairs."
  [lis]
  (monad/monadic
    [n (choose-integer 1 (apply + (map first lis)))]
    (monad/return (pick n lis))))

(defn pick
  "Pick an element from a sequence of (frequency, generator) pairs."
  [n lis]
  (let [f (first lis)
        k (first f)]
    (if (<= n k)
      (second f)
      (recur (- n k) (rest lis)))))


;; Advanced generator combinators
;; ------------------------------

(defn such-that-maybe
  [gen pred]
  (letfn [(mytry [k n]
               (if (= 0 n)
                 (monad/monadic (monad/return nil))
                 (monad/monadic [x (with-tree (resize (+ (* 2 k) n) gen))]
                          (if (pred (tree/tree-outcome x))
                            (monad/return (first (tree/filter-tree pred x)))
                            (mytry (+ k 1) (- n 1))))))]
    (sized (fn [n] (mytry 0 (max 1 n))))))


(defn such-that-generator
  [gen pred]
  (monad/monadic
   [x (maybe-with-tree (such-that-maybe gen pred))]
   (if x
     (monad/return x)
     (sized (fn [n] (resize (+ n 1) (such-that-generator gen pred)))))))


