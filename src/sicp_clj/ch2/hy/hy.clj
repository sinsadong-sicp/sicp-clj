(ns sicp-clj.ch2.hy.hy
	(:use [clojure.contrib.math :only [abs gcd]]))

;2-1
(defn make-rat [n d]
	(let [g (gcd n d)]
		(if (or (and (>= n 0) (> d 0)) (and (< n 0) (< d 0)))
			(vector (/ (abs n) g) (/ (abs d) g))
			(vector (- (/ (abs n) g)) (/ (abs d) g))
			)))
