(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-60
; a0, a1, a2, ... 와 b0, b1, b2, ...를 곱한다면 a0 * (b0, b1, b2, ...) + a1 * (b0, b1, b2, ...) + ...를 하면 된다.

(defn mul-series [s1 s2]
    (cons-stream
        (* (stream-car s1) (stream-car s2))
        (add-streams
            (scale-stream (stream-car s1) (stream-cdr s2))
            (mul-series (stream-cdr s1) (stream-car s2)))
        )
    )
