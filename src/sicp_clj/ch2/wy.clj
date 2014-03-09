(ns sicp-clj.ch2.wy
  (:use [clojure.contrib.math :only [abs gcd]])
  (:use [clojure.contrib.generic.math-functions :only [sgn]]))

; 2-1

(defn make-rational [n d]
  (let [g (gcd n d)
        s (* (sgn n) (sgn d))]
    [(* s (/ (abs n) g)) (/ (abs d) g)]))

; 2-2

(defn make-point [x y]
  [x y])
(defn x-point [p]
  (first p))
(defn y-point [p]
  (second p))

(defn make-segment [p q]
  [p q])
(defn start-of-segment [s]
  (first s))
(defn end-of-segment [s]
  (second s))

(defn midpoint-of-segment [s]
  (defn average [x y]
    (/ (+ x y) 2))
  (let [p (start-of-segment s)
        q (end-of-segment s)
        x (average (x-point p) (x-point q))
        y (average (y-point p) (y-point q))]
    (make-point x y)))
