(ns sicp-clj.ch2.hy.hy
	(:use [clojure.contrib.math :only [abs gcd]]))

(defn cons [x y]
	(defn dispatch [m]
		(cond
			(= m 0) x
			(= m 1) y
			:else (throw (Exception. "Argument not 0 or 1 - CONS"))))
dispatch)

(defn car [z]
	(z 0))
(defn cdr [z]
	(z 1))
(defn average [x y]
	(/ (+ x y) 2))

;2-1
(defn make-rat [n d]
	(let [g (gcd n d)]
		(if (or (and (>= n 0) (> d 0)) (and (< n 0) (< d 0)))
			(cons (/ (abs n) g) (/ (abs d) g))
			(cons (- (/ (abs n) g)) (/ (abs d) g))
			)))

;2-2


(defn make-segment [p1 p2]
	(cons p1 p2))
(defn start-segment [s]
	(car s))
(defn end-segment [s]
	(cdr s))
(defn make-point [x1 y1]
	(cons x1 y1))
(defn x-point [p]
	(car p))
(defn y-point [p]
	(cdr p))
(defn midpoint-segment [s]
	(cons
		(average (x-point (start-segment s)) (x-point (end-segment s)))
		(average (y-point (start-segment s)) (y-point (end-segment s)))))
(defn print-point [p]
	(println "(" (x-point p) "," (y-point p) ")"))
