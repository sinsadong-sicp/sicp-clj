(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-61
(defn negative-stream [st]
    (stream-map (fn [x] (- x)) st))

(defn invert-unit-series [st]
    (cons-stream 1
        (negative-stream
            (mul-series
                (stream-cdr st)
                (invert-unit-series st)))))
