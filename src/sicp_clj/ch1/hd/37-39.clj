(ns sicp-clj.ch1.hd
	(:use clojure.contrib.generic.math-functions))

; 1-37

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

(prn (cont-frac (fn[i] 1.0) (fn[i] 1.0) 100)) ; => 0.6180으로 맞음.
(prn (cont-frac-iter (fn[i] 1.0) (fn[i] 1.0) 100)) ; => 0.6180으로 맞음.

; 1-38

(defn find-e[k]
	(defn n-euler[i] 1.0)
	(defn d-euler[i] ; 1, 2, 1, 1, 4, 1, 1, 6, 1, ...
		(if (= (mod i 3) 2) (* (inc (quot i 3)) 2.0) 1.0))
	(+ 2 (cont-frac n-euler d-euler k))
)

(prn (find-e 100)) ; => 2.71828182845

; 1-39
(def pi-def (. Math PI))

(defn tan-cf[x k]
	(defn n-tan[i]
		(if (= i 1) x (- (sqr x)))); x_1 = x, else -x^2
	(defn d-tan[i] (dec (* 2 i))) ; 1, 3, 5, ...
	(cont-frac n-tan d-tan k)
)

; 아래는 (tan) 함수로 확인했음.
(prn (tan-cf (/ pi-def 4) 100)) ; 1.0
(prn (tan-cf (/ pi-def 3) 100)) ; 1.732 = sqrt(3)