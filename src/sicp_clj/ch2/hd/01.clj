(ns sicp-clj.ch1.hd
  (:use clojure.contrib.generic.math-functions))

(defn gcd[a b]
  (if (zero? b)
    a
    (gcd b (rem a b))))

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
  (defn make-neg [x]
    (if (neg? x) x (- x)))
  (let [g (gcd n d)]
    (if (pos? (* n d))
      [(abs (/ n g)) (abs (/ d g))]
      [(make-neg (/ n g)) (abs (/ d g))])))
