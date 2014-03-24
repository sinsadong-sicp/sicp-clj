(ns sicp-clj.ch2.hy.hy
	(:refer-clojure :exclude [cons])
	(:use [clojure.contrib.math :only [abs gcd expt sqrt]]))

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
(defn square [x]
	(* x x))

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

;2-3
;lower left - upper right
(defn make-rect [p1 p2]
	(cons p1 p2))
(defn lower-left [r]
	(car r))
(defn upper-right [r]
	(cdr r))
(defn area-rect [r]
	(let [width (abs (- (x-point (upper-right r)) (x-point (lower-left r))))]
	(let [height (abs (- (y-point (upper-right r)) (y-point (lower-left r))))]
	(* width height))))
(defn peri-rect [r]
	(let [width (abs (- (x-point (upper-right r)) (x-point (lower-left r))))]
	(let [height (abs (- (y-point (upper-right r)) (y-point (lower-left r))))]
	(sqrt (+ (square width) (square height))))))

;2-4
(defn cons2 [x y]
	(fn [m] (m x y)))
(defn car2 [z]
	(z (fn [p q] p)))
(defn cdr2 [z]
	(z (fn [p q] q)))

;2-5
(defn cons3 [a b]
	(* (expt 2 a) (expt 3 b)))
(defn car3 [z]
	(defn iter [result x]
		(if (= 0 (rem x 2))
			(iter (inc result) (/ x 2))
			result))
	(iter 0 z))
(defn cdr3 [z]
	(defn iter [result x]
		(if (= 0 (rem x 3))
			(iter (inc result) (/ x 3))
			result))
	(iter 0 z))

;2-6
;집합론에서 자연수 정의를 {}, {{}}, {{{}}, {}} 이런식으로 하던거 생각남...
(defn zero []
	(fn [f]
		(fn [x] (x))))
(defn add-1 [n]
	(fn [f]
		(fn [x] (f ((n f) x)))))
(defn one []
	(fn [f]
		(fn [x] (f x))))
(defn two []
	(fn [f]
		(fn [x] (f (f x)))))
(defn add [a b]
	(fn [f]
		(fn [x] ((a f) ((b f) x)))))

;2-7
(defn make-interval [a b]
	(cons a b))
(defn lower-bound [z]
	(car z))
(defn upper-bound [z]
	(cdr z))
(defn add-interval [x y]
	(make-interval
		(+ (lower-bound x) (lower-bound y))
		(+ (upper-bound x) (upper-bound y))))
(defn mul-interval [x y]
	(let [p1 (* (lower-bound x) (lower-bound y))
		p2 (* (lower-bound x) (upper-bound y))
		p3 (* (upper-bound x) (lower-bound y))
		p4 (* (upper-bound x) (upper-bound y))]
	(make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
(defn div-interval [x y]
	(mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))))

;2-8
(defn sub-interval [x y]
	(let [
		 u (- (upper-bound x) (lower-bound y))
		 l (- (lower-bound x) (upper-bound y))]
	(make-interval l u)))

;2-9
(defn width-interval [z]
	(/ (- (upper-bound z) (lower-bound z)) 2))

;2-10
(defn div-interval2 [x y]
	(if (= 0 (width-interval y))
		(throw (Exception. "divided by 0 span interval - div-interval2"))
		(div-interval x y)))
