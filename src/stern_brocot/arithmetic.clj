(ns stern-brocot.arithmetic
  (:require [stern-brocot.tree :refer [L R
                                       SB->N
                                       node->Q
                                       SSB->Q
                                       SB->Q
                                       Q->SSB
                                       flip
                                       inv
                                       neg
                                       fmt]]

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
  (exponential in time and space)"
  ([u] (cons 1 (sqrt u [1])))
  ([u r]
   (lazy-seq
    (let [cc (mul r r)]
      (cond (gt u cc)
            (cons R (sqrt u (conj (vec r) R)))

            (lt u cc)
            (cons L (sqrt u (conj (vec r) L))))))))

(defn SSB->CF
  "Given a sequence of SSB, returns the continued fraction
  representation of the sequence."
  [[s & p]]
  (let [counts (->> p
                    (partition-by identity)
                    (map count))]
    (cons s
          (if (= L (first p))
            (cons 0 counts)
            counts))))

(defn shanks-log
  "Shank's logarithm for integers `a` and `b`."
  ([a b] (cond
           (= a b)
           [1]

           (= b 1)
           (cons 1 (cycle [R]))

           (= a 1)
           [0]

           (< a b)
           (cons 1 (shanks-log a b 0 R))

           :else
           (cons 1 (shanks-log b a 0 L))))
  ([a b n dir]
   (lazy-seq
    (cond (< (Math/pow a (inc n)) b)
          (cons dir (shanks-log a b (inc n) dir))

          (> (Math/pow a (inc n)) b)
          (shanks-log (/ b (Math/pow a n)) a 0 (flip dir))

          :else
          nil))))

(defn SB=
  [[a & as] [b & bs]]
  (cond (not (or a b))
        true

        (not (= a b))
        false

        :else
        (recur as bs)))

(defn SB>
  [[a & as] [b & bs]]
  (cond (not (or a b))
        false

        (not a)
        (= b L)

        (not b)
        (= a R)

        (not (= a b))
        (= b L)

        :else
        (recur as bs)))

(defn SB<
  [[a & as] [b & bs]]
  (cond (not (or a b))
        false

        (not a)
        (= b R)

        (not b)
        (= a L)

        (not (= a b))
        (= b R)

        :else
        (recur as bs)))

(defn SB<=
  [[a & as] [b & bs]]
  (cond (not (or a b))
        true

        (not a)
        (= b R)

        (not b)
        (= a L)

        (not (= a b))
        (= b R)

        :else
        (recur as bs)))

(defn SB>=
  [[a & as] [b & bs]]
  (cond (not (or a b))
        true

        (not a)
        (= b L)

        (not b)
        (= a R)

        (not (= a b))
        (= b L)

        :else
        (recur as bs)))

(defn SSB=
  [[sa & a] [sb & b]]
  (if (= sa sb)
    (SB= a b)
    false))

(defn SSB<
  [[sa & a] [sb & b]]
  (cond (< sa sb)
        true

        (> sa sb)
        false

        :else
        (SB< a b)))

(defn SSB>
  [[sa & a] [sb & b]]
  (cond (< sa sb)
        false

        (> sa sb)
        true

        :else
        (SB> a b)))

(defn SSB<=
  [[sa & a] [sb & b]]
  (cond (< sa sb)
        true

        (> sa sb)
        false

        :else
        (SB<= a b)))

(defn SSB>=
  [[sa & a] [sb & b]]
  (cond (< sa sb)
        false

        (> sa sb)
        true

        :else
        (SB>= a b)))

(defn log
  ([a b]
   (cond (SSB= a b)
         [1]

         (SSB= a [1])
         (cons 1 (cycle [R]))

         (SSB= b [1])
         [0]

         (SSB< a [1])
         (cond (SSB< b [1])
               (let [a-inv (inv a)
                     b-inv (inv b)]
                 (if (SSB< a-inv b-inv)
                   (cons 1 (log a-inv b-inv [1] R))
                   (cons 1 (log b-inv a-inv [1] L))))

               (SSB> b [1])
               (let [a-inv (inv a)]
                 (if (SSB< a-inv b)
                   (cons -1 (log a-inv b [1] R))
                   (cons -1 (log b a-inv [1] L)))))

         (SSB> a [1])
         (cond (SSB< b [1])
               (let [b-inv (inv b)]
                 (if (SSB< a b-inv)
                   (cons -1 (log a b-inv [1] R))
                   (cons -1 (log b-inv a [1] L))))

               (SSB> b [1])
               (if (SSB< a b)
                 (cons 1 (log a b [1] R))
                 (cons 1 (log b a [1] L))))))
  ([a b mem dir]
   (let [next-mem (mul mem a)]
     #_(println
        (str "Called with:\n"
             "a:        " (SSB-str a) "\n"
             "b:        " (SSB-str b) "\n"
             "mem:      " (SSB-str mem) "\n"
             "next-mem: " (SSB-str next-mem) "\n"
             "dir:      " (fmt [dir]) "\n"))
     (lazy-seq
      (cond (SSB< next-mem b)
            (cons dir (log a b next-mem dir))

            (SSB> next-mem b)
            (log (div b mem) a [1] (flip dir)))))))

(defn exp
  "Greedy exponential using [[log]]."
  ([a b]
   (cond (SSB= a [1])
         [1]

         (SSB< a [1])
         (inv (cons 1 (exp (inv a) b [1])))

         (SSB< b [0])
         (inv (cons 1 (exp a (neg b) [1])))

         :else
         (cons 1 (exp a b [1]))))
  ([a b r]
   (lazy-seq
    (cond (SSB< (log a r) b)
          (cons R (exp a b (conj r R)))

          (SSB> (log a r) b)
          (cons L (exp a b (conj r L)))))))

(defn entropy
  [[s & bs :as b]]
  (let [e (cons 1 (map (constantly R) (rest bs)))
        r (div (sub (->> bs (filter #{R}) (cons 1)) [1]) e)
        l (div (sub (->> bs (filter #{L}) (map flip) (cons 1)) [1]) e)]
    (neg
     (add
      (mul (log [1 R] r) r)
      (mul (log [1 R] l) l)))))
