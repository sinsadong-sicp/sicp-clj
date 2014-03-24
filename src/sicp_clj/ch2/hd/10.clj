(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only []])
  (:use [clojure.contrib.generic.math-functions :only []]))

; 2-10
(defn make-interval [a b] [a b])
(defn lower-bound [i] (first i))
(defn upper-bound [i] (last i))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn div-interval [x y]
    (if (>= 0 (* (upper-bound y) (lower-bound y)))
        (prn "Error: the divider range spans to 0")
        (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))
