(ns stern-brocot.arithmetic
  (:require [stern-brocot.tree :refer [L R
                                       SSB->Q
                                       Q->SSB]]
            [stern-brocot.bihomographic :refer [bihom]]))

(defn add
  "Given two or more lazy sequences of SSB, returns the lazy sequence of
  the sum on SSB."
  [& us]
  (reduce (partial bihom [0N 1N 1N 0N
                          0N 0N 0N 1N]) us))

(defn sub
  "Given two or more lazy sequences of SSB, returns the lazy sequence of
  the difference on SSB."
  [& us]
  (reduce (partial bihom [0N 1N -1N 0N
                          0N 0N 0N 1N]) us))

(defn mul
  "Given two or more lazy sequences of SSB, returns the lazy sequence of
  the product on SSB."
  [& us]
  (reduce (partial bihom [1N 0N 0N 0N
                          0N 0N 0N 1N]) us))

(defn div
  "Given two or more lazy sequences of SSB, returns the lazy sequence of
  the division on SSB."
  [& us]
  (reduce (partial bihom [0N 1N 0N 0N
                          0N 0N 1N 0N]) us))

(defn- euler-seq
  "Helper function, generates the tail of the e sequence."
  [n]
  (lazy-seq
   (concat (if (zero? (mod n 2))
             `(~R ~@(repeat (* 2 n) L) ~R)
             `(~L ~@(repeat (* 2 n) R) ~L))
           (euler-seq (inc n)))))

(def euler
  "e, the base of the natural logarithm."
  (cons 1 (euler-seq 0)))

(defn- gt'
  [[a & u'] [b & v']]
  (cond (and a b (= a b))
        (gt' u' v')

        (or (and (= a R) (= b L))
            (and (nil? a) (= b L))
            (and (nil? b) (= a R)))
        true

        :otherwise
        false))

(defn gt
  "Given two sequences of SSB `u` and `v`, returns true if `u` is greater
  than `v`."
  [[su & u] [sv & v]]
  (cond (< su sv)
        false

        (< sv su)
        true

        (pos? sv)
        (gt' u v)

        :otherwise
        (gt' v u)))

(defn lt
  "Given two sequences of SSB `u` and `v`, returns true if `u` is lesser
  than `v`."
  [u v]
  (gt v u))

(defn sqrt
  "Given a sequence of SSB, returns the square root of the sequence on SSB.
  (very slow and space consuming)"
  ([u] (cons 1 (sqrt u [1])))
  ([u r]
   (lazy-seq
    (let [cc (mul r r)]
      (cond (gt u cc)
            (cons R (sqrt u (conj (vec r) R)))

            (lt u cc)
            (cons L (sqrt u (conj (vec r) L))))))))
