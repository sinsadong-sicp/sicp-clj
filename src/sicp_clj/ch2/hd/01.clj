(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only [abs gcd]])
  (:use [clojure.contrib.generic.math-functions :only [sgn]]))

; (defn make-rat [n d]
;   (let [g (gcd n d)]
;     [(/ n g) (/ d g)] ))

(defn numer [x] (first x))

(defn denom [x] (last x))

(defn print-rat [x]
  (printf "%d/%d" (numer x) (denom x))
  (newline))

; 2-1
(defn make-rat [n d]
  (let [g (gcd n d)
    sign (* (sgn n) (sgn d))]
    [(* sign (abs (/ n g))) (abs (/ d g))] ))
