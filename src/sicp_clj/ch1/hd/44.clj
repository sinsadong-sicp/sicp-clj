(ns sicp-clj.ch1.hd
  (:use clojure.contrib.generic.math-functions))

; 1-44
(def dx 0.00001)

(defn compose[f g]
  (fn[x] (f (g x))))

(defn repeated[f n]
  (defn iter[result-fn counter]
    (if (= counter 1)
      result-fn
      (iter (compose f result-fn) (dec counter))))
  (iter f n))

(defn smooth[f]
  (fn[x]
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(defn n-smooth[f n]
  (repeated smooth n))

; 1-45, 46 pass
