(ns sicp-clj.ch1.hd
	(:use clojure.contrib.generic.math-functions))

(defn cont-frac[n d k]
	(defn recursion[counter]
		(if (> counter k) 0
			(/ (n counter) (+ (d counter) (recursion (inc counter)))))
	)
	(recursion 1)
)

(defn cont-frac-iter[n d k]
	(defn iter[counter result]
		(if (> counter k) result
			(iter (inc counter) (/ (n counter) (+ (d counter) result))))
	)
	(iter 1 0)
)

(prn (cont-frac (fn[i] 1.0) (fn[i] 1.0) 100)) ; => 1.6180으로 맞음.
(prn (cont-frac-iter (fn[i] 1.0) (fn[i] 1.0) 100)) ; => 1.6180으로 맞음.