(ns stern-brocot.homographic
  (:require [stern-brocot.tree :refer [L R sgn Q->SB Q->SSB]]))

(defn det
  "Determinant of a homographic map."
  [[a b c d]]
  (- (* a d) (* b c)))

(defn- lin-sgn
  "Helper function to determine the sign of a function ax+b for x>0. If
  a and b have opposite signs, the function returns false, otherwise
  the sign is determined and the function returns the sign."
  [a b]
  (if (= 0 a b)
    0
    (let [sum (+ (sgn a) (sgn b))]
      (cond (pos? sum)
            1

            (neg? sum)
            -1

            :otherwise
            false))))

(defn- nom-sgn
  "`lin-sgn` of the nominator of H."
  [[a b _ _]]
  (lin-sgn a b))

(defn- denom-sgn
  "`lin-sgn` of the denominator of H."
  [[_ _ c d]]
  (lin-sgn c d))

(defn hom-sgn
  "Given a homographic map and a sequence of SB, `hom-sgn` returns the
  sign of the evaluated homographic map, the map itself and the rest of
  the sequence.

      [sign H u]"
  [[a b c d :as H] [f & u' :as u]]
  (if (empty? u)
    [(* (sgn (+ a b)) (sgn (+ c d))) H []]
    (let [nom (nom-sgn H)
          denom (denom-sgn H)]
      (if (and nom denom)
        [(* nom denom) H u]
        (recur (f H) u')))))

(defn emit?
  "True if the homographic map allows to emit a term, false otherwise."
  [a b c d]
  (or (and (<= c a) (< d b))
      (and (< c a)  (<= d b))))

(defn- U
  "Acts on the homographic map if it can emit an `R`."
  [[a b c d]]
  [(- a c) (- b d)
   c       d])

(defn- D
  "Acts on the homographic map if it can emit an `L`."
  [[a b c d]]
  [a       b
   (- c a) (- d b)])

(defn hom-emit
  "Given a holographic map and a sequence of SB, tries to emit an SB
  sequence term or absorbs a term from the SB sequence."
  [[a b c d :as H] u]
  (lazy-seq
   (cond (empty? u)
         (Q->SB (+ a b) (+ c d))

         (emit? a b c d)
         (cons R (hom-emit (U H) u))

         (emit? c d a b)
         (cons L (hom-emit (D H) u))

         :otherwise
         (let [[f & u'] u]
           (hom-emit (f H) u')))))

(defn hom'
  "Given a homographic map and a sequence of SB, emits the
  representation of the homographic map on SSB."
  [[a b c d :as H] u]
  (lazy-seq
   (if (zero? (det H))
     (Q->SSB (/ (+ a b) (+ c d)))
     (let [[sign [a' b' c' d' :as H'] u'] (hom-sgn H u)]
       (cond (zero? sign)
             [0]

             (pos? sign)
             (cons 1 (hom-emit (if (pos? (+ a' b'))
                                 H'
                                 [(- a') (- b') (- c') (- d')]) u'))

             (neg? sign)
             (cons -1 (hom-emit (if (pos? (+ a' b'))
                                  [a' b' (- c') (- d')]
                                  [(- a') (- b') c' d']) u')))))))

(defn hom
  "Given a homographic map and a sequence of SSB, emits the
  representation of the homographic map on SSB."
  [[a b c d :as H] [sign & u]]
  (lazy-seq
   (condp = sign
     0 [0]
     1 (hom' H u)
     -1 (hom' [(- a) b (- c) d] u))))
