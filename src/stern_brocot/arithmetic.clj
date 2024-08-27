(ns stern-brocot.arithmetic
  (:require [stern-brocot.tree :refer [L R
                                       SB->N
                                       SSB->Q
                                       SB->Q
                                       Q->SSB
                                       fmt]]
            stern-brocot.pi
            [stern-brocot.bihomographic :refer [bihom]]
            [clojure.java.io :as io]))

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

(defn combs
  [acc n]
  (if (pos? n)
    (recur (mapcat (fn [x]
                     [(conj x L)
                      (conj x R)])
                   acc)
           (dec n))
    acc))

(map fmt (mapcat #(combs [[]] %) (range 7)))

(defn combs2
  [n]
  (if (pos? n)
    (mapcat (fn [x]
              [(conj x L)
               (conj x R)])
            (combs2 (dec n)))
    [[]]))

(defn combs2
  [n]
  (if (pos? n)
    (mapcat (fn [x]
              [(conj x L)
               (conj x R)])
            (combs2 (dec n)))
    [[]]))

#_(sort (map SB->Q (mapcat #(combs2 %) (range 10))))

(defn num->pt
  [num]
  (format "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" />"
          (/ 1 (double (SB->Q num)))
          1.0 #_(double (SB->Q num))
          #_(Math/exp (/ (double (SB->Q num)) (count num)))
          (/ 0.05 (inc (count (drop-while #{R} num)))) #_(- (double (SB->Q num)) (Math/floor (double (SB->Q num))))))

{:id 1
 :value []
 :children nil}

(defn nodes
  [k ns]
  (if (zero? k)
    ns
    (concat ns
            (nodes (dec k)
                   (mapcat (fn [x] [(conj x L)
                                    (conj x R)])
                           ns)))))

(comment
  ;; should work with a sane number system, doesn't work in clj, prolly not in any envinronment
  (defn bin
    [ns nbins]
    (let [max-n (apply max ns)
          dn (/ max-n nbins)]
      (partition-by #(mod % dn) ns))))

(defn bin*
  [ns nm bin-size]
  (let [[a b] (split-with #(< % nm) ns)]
    (lazy-cat [a] (bin* b (+ nm bin-size) bin-size))))

(defn bin
  [ns bin-size]
  (bin* ns bin-size bin-size))

#_(take 10 (bin (range 10) 1.0))

#_(SB->N [R L R L R])

(comment
  (def nums
    (->> (nodes 15 [[]])
         (filter #(< (count (drop-while #{R} %)) 4))
         (sort-by SB->Q))))

(comment
  (def NS
    (nodes 22 [[]]))

  (def NSQ
    (sort (map SB->Q NS))))

(comment
  (let [k 22
        d 0.3
        f (map count (take (Math/ceil (/ k d)) (bin NSQ d)))
        m (apply max f)]
    #_(map #(/ (Math/log %1) (Math/log %2)) f (rest f))
    #_(map (fn [x]
             (apply str (repeat (/ (* 120 x) m) "|")))
           f)
    (with-open [w (clojure.java.io/writer "/home/timr/foo-02.csv")]
      (doseq [[a b] (map-indexed (fn [a b]
                                   [(* d a) b])
                                 f)]
        (.write w (str a "," b "\n"))))))

(comment

  (let [rnums (map SB->Q nums)]
    (reductions (fn [acc next]
                  (if (empty? acc)
                    [0]
                    [next]))
                []
                rnums))

  (time
   (with-open [w (io/writer "/home/timr/foo.svg")]
     (spit w
           (format "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 10 10\">%s</svg>"
                   (->> nums
                        (map num->pt)
                        (apply str)))))))

(def pi
  (into [1]
        (->> stern-brocot.pi/pi-cf
             rest
             (take-nth 2)
             (partition 2)
             (mapcat (fn [[nr nl]]
                       [(repeat nr R)
                        (repeat nl L)]))
             (apply concat))))

(defn energy
  [s]
  (dec (count s)))

(defn entropy-old
  [s]
  (let [e (energy s)
        nr (->> s (filter #{R}) count)
        nl (->> s (filter #{L}) count)]
    (cond (zero? e)
          0

          (zero? nr)
          0

          (zero? nl)
          0

          :else
          (- (/ (+ (* (/ nr e) (Math/log (/ nr e)))
                   (* (/ nl e) (Math/log (/ nl e))))
                (Math/log 2))))))

(defn safe-log
  [x]
  (if (zero? x) 0 (Math/log x)))

(def phi (into [1] (comp
                    (take 10000)
                    (mapcat identity))
               (repeat [R L])))

(def sqrt2 (into [1 R] (apply concat (repeat 50000 [L L R R]))))

(def sqrt3 (into [1 R] (apply concat (repeat 80000 [L R R]))))

(defn flip
  [dir]
  (if (= dir R) L R))

(defn pow
  ([base exponent]
   (pow base (rest exponent) [1] R))
  ([b e mem dir]
   (let [[e & es] e]
     (lazy-seq
      (cond (= e R)
            (concat (repeat (SSB->Q mem) dir) (pow (add b mem) es (concat mem (repeat (SSB->Q mem) dir)) dir))

            (= e L)
            (concat (repeat (SSB->Q mem) dir) (pow (add mem b) es (concat mem (repeat (SSB->Q mem) (flip dir))) (flip dir)))

            :else
            (repeat (SSB->Q mem) dir))))))

(let [x (pow [1 R] [1 R R])]
  [(SB->Q x)
   (fmt x)])

(defn log
(defn shanks-log
  "Shank's logarithm for integers `a` and `b`."
  ([b a] (cond
           (= a b)
           [1]

           (= a 1)
           [0]

           (< a b)
           (cons 1 (shanks-log b a 0 R))

           :else
           (cons 1 (shanks-log a b 0 L))))
  ([a b n dir]
   (lazy-seq
    (cond (< (Math/pow a (inc n)) b)
          (cons dir (shanks-log a b (inc n) dir))

          (> (Math/pow a (inc n)) b)
          (shanks-log (/ b (Math/pow a n)) a 0 (flip dir))

          :else
          nil))))

(defn log
  [b a]
  (let [I [1 0 0 1]
        [a-sign & as] a
        [b-sign & bs] b
        [aa ab ac ad] (reduce (fn [n b] (b n)) I as)
        [ba bb bc bd] (reduce (fn [n b] (b n)) I bs)
        [x y] [(+ aa ab) (+ ac ad)]
        [n d] [(+ ba bb) (+ bc bd)]]
    (cond (= x 1)
          (div [1] (sub (shanks-log y d) (shanks-log y n)))

          :else
          (div (sub [1] (shanks-log x y))
               (sub (shanks-log x n) (shanks-log x d))))))

(double (SSB->Q (log [1 R R R R R R R R R] [1 R R R R R R])))

(comment
  (double (SSB->Q sqrt2))

  (entropy-old (take 100 sqrt2))
  (entropy-old (take 100 phi))

  (take 2 phi)

  (with-open [w (io/writer "/home/timr/sqrt2_entropy.dat")]
    (doseq [k (range 1 80000)]
      (.write w (str k
                     " "
                     (->> (take k sqrt2)
                          entropy-old)
                     "\n"))))

  (conj [1 2 3] 3)

  (take-nth 2)

  (count pi))

(double (SSB->Q (log 2 3)))

(defn inspect
  [x]
  (let [y (take 20 x)]
    (println (fmt y) " " (double (SSB->Q y)))))

(defn entropy
  [s]
  (let [e (energy s)
        es (Q->SSB e)
        r (->> s (filter #{R}) count)
        rs (Q->SSB r)
        l (->> s (filter #{L}) count)
        ls (Q->SSB l)]
    (div
     (sub (mul es (log 2 e))
          (add (mul rs (log 2M r)) (mul ls (log 2M l))))
     es)))

#_(div
   (sub (mul [1] [1 R R])
        (add (mul [1 L] [1 R]) (mul [1 L] [1 R])))
   [1 R R R])

(fmt (log 2 10))
(double (SSB->Q (log 10 2)))

(comment
  (let [x (entropy (take 20 phi))]
    [(count (fmt x))
     (double (SSB->Q x))]))

;; (take 10 (entropy [R R R L]))


