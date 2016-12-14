(ns arithmetic
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

(defn fmt
  [u]
  (apply str (map {-1 "-"
                   0  "0"
                   1  nil
                   L  "L"
                   R  "R"} u)))
