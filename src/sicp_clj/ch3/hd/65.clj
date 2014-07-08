(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-65
(defn ln-sum [n]
    (cons-stream (/ 1.0 n)
        (stream-map - (ln-sum (inc n)))))

(def ln-stream
    (partial-sums (ln-sum 1)))
