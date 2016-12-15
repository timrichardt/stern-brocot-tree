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
  the multiplication on SSB."
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
  `(1 ~R ~R ~@(euler-seq 1)))
