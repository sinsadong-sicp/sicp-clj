(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 2-83
(defn raise [x] (apply-generic 'raise x))
; scheme-number에 추가
(put 'raise 'integer (fn [x] (make-rational x 1)))
; rational에 추가
(put 'raise 'rational (fn [x] (make-real (/ (numer x) (denom x)))))
; real에 추가
(put 'raise 'real (fn [x] (make-from-real-imag x 0)))
