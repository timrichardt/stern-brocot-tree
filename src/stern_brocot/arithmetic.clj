(ns stern-brocot.arithmetic
  (:require [stern-brocot.tree :refer [L R
                                       SB->N
                                       node->Q
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

(defn llog
  ([a b]
   (cond (SSB= a b)
         [1]

         (SSB< a b)
         (if (SSB<= a [1])
           (div [1] (sub [1] (cons 1 (llog (div [1] a) b [1] L))))
           ;; (cons -1 (llog (div [1] a) b [1] L))
           (cons 1 (llog a b [1] R)))

         :else
         (if (SSB<= b [1])
           (cons -1 (llog a (div [1] b) [1] L))
           (cons 1 (llog b a [1] R)))))
  ([a b mem dir]
   (let [next-mem (mul a mem)]
     (lazy-seq
      (cond (SSB< next-mem b)
            (cons dir (llog a b next-mem dir))

            (SSB> next-mem b)
            (llog (div b next-mem) a [1] (flip dir))

            :else
            nil)))))

(defn SB-invert
  [a]
  (map {L R R L} a))

(defn SSB-invert
  [[s & a]]
  (cons s (SB-invert a)))

(defn SSB-str
  [a]
  (let [y (take 40 a)]
    (format "%40s %f" (fmt y) (double (SSB->Q y)))))

(defn llog
  ([a b]
   (cond (SSB= a b)
         [1]

         (SSB= a [1])
         (cons 1 (cycle [R]))

         (SSB= b [1])
         [0]

         (SSB< a [1])
         (cond (SSB< b [1])
               (let [a-inv (SSB-invert a)
                     b-inv (SSB-invert b)]
                 (if (SSB< a-inv b-inv)
                   (cons -1 (llog a-inv b-inv [1] R))
                   (cons 1 (llog b-inv a-inv [1] L))))

               :else
               (let [a-inv (SSB-invert a)]
                 (if (SSB< a-inv b)
                   (cons -1 (llog a-inv b [1] R))
                   (cons 1 (llog b a-inv [1] L)))))

         (SSB> a [1])
         (cond (SSB< b [1])
               (let [b-inv (SSB-invert b)]
                 (if (SSB< a b-inv)
                   (cons -1 (llog a b-inv [1] R))
                   (cons -1 (llog b-inv a [1] L))))

               :else
               (if (SSB< a b)
                 (cons 1 (llog a b [1] R))
                 (cons 1 (llog b a [1] L))))))
  ([a b mem dir]
   (let [next-mem (mul mem a)]
     (println (str "Called with:\n"
                   "a:        " (SSB-str a) "\n"
                   "b:        " (SSB-str b) "\n"
                   "mem:      " (SSB-str mem) "\n"
                   "next-mem: " (SSB-str next-mem) "\n"
                   "dir:      " (fmt [dir]) "\n"))
     (lazy-seq
      (cond (SSB< next-mem b)
            (cons dir (llog a b next-mem dir))

            (SSB> next-mem b)
            (llog (div b mem) a [1] (flip dir))

            :else
            nil)))))

(comment
  (let [a [1 R]
        b [1 R L L]
        x (take 40 (llog a b))]
    (def y
      [(double (SSB->Q (take 20 a)))
       (SSB> a [1])
       (double (SSB->Q (take 20 b)))
       (SSB> b [1])
       (double (SSB->Q x))
       (fmt x)])
    y))

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
     (sub (mul es (shanks-log 2 e))
          (add (mul rs (shanks-log 2M r)) (mul ls (shanks-log 2M l))))
     es)))

