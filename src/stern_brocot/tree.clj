(ns stern-brocot.tree)

;; This file implements the core structure of the unsigned and signed
;; Stern-Brocot tree.

;; A node of the tree is represented as a 4-vector of integers.

(def I
  "Root node of the tree."
  [1N 0N
   0N 1N])

(defn L "Given a node, returns the left child."
  [[a b c d]]
  [(+ a b) b
   (+ c d) d])

(defn R "Given a node, returns the right child"
  [[a b c d]]
  [a (+ a b)
   c (+ c d)])

(defn node->Q
  "Given a node, returns the corresponding fraction."
  [[a b c d]]
  (/ (+ a b) (+ c d)))

;; Any positive rational number r (element of Q+) can now be represented
;; as a sequential application of L and R on the root of the tree. The
;; set of all sequences is called SB. We represent a sequence of SB as a
;; lazy sequence of the elements L and R
;;
;;     [L R L L L R]

(defn SB->Q
  "Given a sequence of SB, returns the corresponding element of Q+."
  [u]
  (node->Q (reduce (fn [n b] (b n)) I (vec u))))

(defn Q->SB
  "Given a positive rational number, returns the corresponding element
  of SB."
  ([r]
   (let [r (rationalize r)]
     (if (integer? r)
       (Q->SB r 1)
       (let [n (numerator r)
             d (denominator r)]
         (Q->SB n d)))))
  ([n d]
   (lazy-seq
    (cond (< n d)
          (cons L (Q->SB n (- d n)))

          (< d n)
          (cons R (Q->SB (- n d) d))

          :otherwise
          []))))

;; SB can be extended to include non-positive rational numbers by
;; introducing a sign information. We represent elements of the signed
;; Stern-Brocot tree (SSB) as elements of SSB with an additional sign
;; number in the zeroth place.
;;
;;     [1 L R R L]    positive
;;     [0]            zero
;;     [-1 L R R L]   negative

(defn SSB->Q
  "Given a sequence on SSB, returns the corresponding element of Q."
  [[s & u]]
  (* s (SB->Q u)))

(defn Q->SSB
  "Given a rational number, returns the corresponding element of SSB."
  [r]
  (if (zero? r)
    [0]
    (let [r (rationalize r)]
      (if (integer? r)
        (if (pos? r)
          (cons 1 (Q->SB r 1))
          (cons -1 (Q->SB (- r) 1)))
        (let [n (numerator r)
              d (denominator r)]
          (if (pos? r)
            (cons 1 (Q->SB n d))
            (cons -1 (Q->SB (- n) d))))))))

(defn sgn
  "Given a number or a sequence of SSB, returns it's sign."
  [x]
  (if (coll? x)
    (first x)
    (cond (pos? x)
          1

          (neg? x)
          -1

          :otherwise
          0)))

(defn fmt
  [u]
  (apply str (map {-1 "-"
                   0  "0"
                   1  nil
                   L  "L"
                   R  "R"} u)))

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
