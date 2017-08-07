(ns stern-brocot.spike
  (:require [stern-brocot.tree :refer [R L SSB->CF]]
            [stern-brocot.arithmetic :refer [mul div sqrt euler]]
            [hiccup.core :refer [html]]
            [clojure.java.io :as io]))

(defn path
  [[s & cf]]
  (let [dim 4
        cf (take 300 (partition 2 (interleave (cycle [true false]) cf)))]
    (reduce (fn [[x y s] [right? d]]
              (if right?
                [(+ x (* dim d))
                 y
                 (str s "L" (+ x (* dim d)) " " y)]
                [x
                 (+ y (* dim d))
                 (str s "L" x " " (+ y (* dim d)))]))
            [0 0 "M0 0"] cf)))

(defn svg
  []
  [:svg {:width 4000, :height 2001
         :xmlns "http://www.w3.org/2000/svg"
         :version "1.1"}
   [:rect {:x 0, :y 0, :height 2001, :width 4000}]
   [:line {:x1 0, :x2 4000, :y1 2000, :y2 2000
           :stroke "black"
           :stroke-width 2}]
   (for [n (range 50)]
     [:g
      [:path {:d (nth (path (SSB->CF (sqrt (cons 1 (repeat (inc n) R))))) 2)
              :fill "none"
              :stroke "white"
              :stroke-opacity 0.2
              :stroke-width 2}]

      [:path {:d (nth (path (SSB->CF (div [1] (sqrt (cons 1 (repeat (inc n) R)))))) 2)
              :fill "none"
              :stroke "white"
              :stroke-opacity 0.2
              :stroke-width 2}]])

   [:path {:d (nth (path (SSB->CF euler)) 2)
           :fill "none"
           :stroke "red"
           :stroke-opacity 0.2
           :stroke-width 3}]

   [:path {:d (nth (path (SSB->CF (div [1] euler))) 2)
           :fill "none"
           :stroke "red"
           :stroke-opacity 0.2
           :stroke-width 3}]

   [:path {:d (nth (path (SSB->CF (mul euler euler))) 2)
           :fill "none"
           :stroke "blue"
           :stroke-opacity 0.2
           :stroke-width 4}]

   [:path {:d (nth (path (SSB->CF (div [1] (mul euler euler)))) 2)
           :fill "none"
           :stroke "blue"
           :stroke-opacity 0.2
           :stroke-width 4}]])

(defn render
  []
  (with-open [w (io/writer "/home/tim/sqrts.svg")]
    (.write w (html (svg)))))

(render)
