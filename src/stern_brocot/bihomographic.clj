(ns stern-brocot.bihomographic
  (:require [stern-brocot.tree :refer [sgn L R Q->SSB]]
            [stern-brocot.homographic :refer [hom-sgn hom-emit]]))

(defn LL
  [[a b c d
    e f g h]]
  [(+ a b c d) (+ b d) (+ c d) d
   (+ e f g h) (+ f h) (+ g h) h])

(defn LR
  [[a b c d
    e f g h]]
  [(+ a c) (+ a b c d) c (+ c d)
   (+ e g) (+ e f g h) g (+ g h)])

(defn RL
  [[a b c d
    e f g h]]
  [(+ a b) b (+ a b c d) (+ b d)
   (+ e f) f (+ e f g h) (+ f h)])

(defn RR
  [[a b c d
    e f g h]]
  [a (+ a b) (+ a c) (+ a b c d)
   e (+ e f) (+ e g) (+ e f g h)])

(defn ssg
  [a b c d]
  (+ (sgn a) (sgn b) (sgn c) (sgn d)))

(declare absorb)

(defn- bihom-sgn'
  [[a b c d e f g h :as B]
   u
   v]
  (let [nom-ssg (ssg a b c d)
        denom-ssg (ssg e f g h)]
    (cond (empty? u)
          (let [[s [a' b' c' d' :as H] v']
                (hom-sgn [(+ a c) (+ b d)
                          (+ e g) (+ f h)] v)]
            [s [0 0 a' b'
                0 0 c' d'] [] v'])

          (empty? v)
          (let [[s [a' b' c' d' :as H] u']
                (hom-sgn [(+ a b) (+ c d)
                          (+ e f) (+ g h)] u)]
            [s [0 a' 0 b'
                0 c' 0 d'] u' []])

          (= 0 b c d)
          (cond (= 0 f g h)
                [(* (sgn a) (sgn e)) B]

                (> nom-ssg 2)
                [(sgn a) B u v]

                (< denom-ssg -2)
                [(- (sgn a) B u v)]

                :else
                (absorb B u v))

          :else
          (cond (= 0 f g h)
                (cond (> denom-ssg 2)
                      [(sgn e) B u v]

                      (< nom-ssg -2)
                      [(- (sgn e) B u v)]

                      :else
                      (absorb B u v))

                :else
                (cond (> (* nom-ssg denom-ssg) 8)
                      [1 B u v]

                      (< (* nom-ssg denom-ssg) -8)
                      [-1 B u v]

                      :else
                      (absorb B u v))))))

(defn absorb
  [B [f & u] [g & v]]
  (cond (= f g L)
        (bihom-sgn' (LL B) u v)

        (= f g R)
        (bihom-sgn' (RR B) u v)

        (and (= f L) (= g R))
        (bihom-sgn' (LR B) u v)

        (and (= f R) (= g L))
        (bihom-sgn' (RL B) u v)))

(defn bihom-sgn
  [B u v]
  (trampoline bihom-sgn' B u v))

(defn emit?
  [a b c d
   e f g h]
  (or (and (<= e a) (<= f b) (<= g c) (< h d))
      (and (<= e a) (<= f b) (< g c)  (<= h d))
      (and (<= e a) (< f b)  (<= g c) (<= h d))
      (and (< e a)  (<= f b) (<= g c) (<= h d))))

(defn BU
  [[a b c d
    e f g h]]
  [(- a e) (- b f) (- c g) (- d h)
   e       f       g       h])

(defn BD
  [[a b c d
    e f g h]]
  [a       b       c       d
   (- e a) (- f b) (- g c) (- h d)])

(defn bihom-emit
  [[a b c d
    e f g h :as B] u v]
  (lazy-seq
   (cond (empty? u)
         (hom-emit [(+ a c) (+ b d)
                    (+ e g) (+ f h)] v)

         (empty? v)
         (hom-emit [(+ a b) (+ c d)
                    (+ e f) (+ g h)] u)

         :else
         (cond (emit? a b c d e f g h)
               (cons R (bihom-emit (BU B) u v))

               (emit? e f g h a b c d)
               (cons L (bihom-emit (BD B) u v))

               :else
               (let [[f & u'] u
                     [g & v'] v]
                 (cond (= f g L)
                       (bihom-emit (LL B) u' v')

                       (= f g R)
                       (bihom-emit (RR B) u' v')

                       (and (= f L) (= g R))
                       (bihom-emit (LR B) u' v')

                       (and (= f R) (= g L))
                       (bihom-emit (RL B) u' v')))))))

(defn same-ratio?
  [[a b c d
    e f g h]]
  (and (= (* a f) (* b e))
       (= (* b g) (* c f))
       (= (* c h) (* d g))
       (= (* a g) (* c e))
       (= (* a h) (* d e))
       (= (* b h) (* d f))))

(defn Bs-
  [[a b c d
    e f g h]]
  [(- a) (- b) (- c) (- d)
   (- e) (- f) (- g) (- h)])

(defn Bu-
  [[a b c d
    e f g h]]
  [a b c d
   (- e) (- f) (- g) (- h)])

(defn Bv-
  [[a b c d
    e f g h]]
  [(- a) (- b) (- c) (- d)
   e f g h])

(defn bihom'
  [[a b c d
    e f g h :as B] u v]
  (lazy-seq
   (cond (same-ratio? B)
         (cond (not (= h 0))
               (Q->SSB (/ d h))

               (not (= g 0))
               (Q->SSB (/ c g))

               (not (= f 0))
               (Q->SSB (/ b f))

               :else
               (Q->SSB (/ a e)))

         :else
         (let [[s [a' b' c' d'
                   e' f' g' h' :as B']
                u' v'] (bihom-sgn B u v)]
           (cond (zero? s)
                 [0]

                 (= s 1)
                 (cons 1 (bihom-emit (if (pos? (+ a' b' c' d'))
                                       B'
                                       (Bs- B'))
                                     u' v'))

                 (= s -1)
                 (cons -1 (bihom-emit (if (pos? (+ a' b' c' d'))
                                        (Bu- B')
                                        (Bv- B'))
                                      u' v')))))))

(defn bihom
  [[a b c d
    e f g h] [su & u] [sv & v]]
  (bihom' [(* su sv a) (* su b) (* sv c) d
           (* su sv e) (* su f) (* sv g) h] u v))

;; TODO: - laziest absorbtion strategy, more compact decision tree

#_(defn- quad-sgn
    "Helper function to determine the sign of a function ax+b for x>0. If
  a and b have opposite signs, the function returns false, otherwise
  the sign is determined and the function returns the sign."
    [a b c d]
    (if (= 0 a b)
      0
      (let [sum (+ (sgn a) (sgn b) (sgn c) (sgn d))]
        (cond (pos? sum)
              1

              (neg? sum)
              -1

              :else
              false))))
