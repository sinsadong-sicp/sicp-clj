(ns sicp-clj.ch1.hy.hytest
	(:use sicp-clj.ch1.hy.hy))

(def tol 0.0001)

(defn check-with-error [a b]
	(if (< (abs (- a b)) tol)
		true
		false)
)

(defn hytest-single [check fname a b]
	(if (check a b)
		(println "PASS")
		(println (str "FAIL " fname " expected : " a " got : " b))))

(def hytest-with-error
	(partial hytest-single check-with-error))
(def hytest
	(partial hytest-single =))

;1-3
(hytest "1-3" 13 (sum-of-squares-of-larger-two 1 2 3) )
(hytest "1-3" 20 (sum-of-squares-of-larger-two 4 1 2))
(hytest "1-3" 5 (sum-of-squares-of-larger-two -1 -2 -3))

;1-7
(hytest "1-7" 4.0 (sqrt 16))
(hytest-with-error "1-7" 3.6055 (sqrt 13))

;1-8
(hytest-with-error "1-8" 3.0013 (cuberoot 27))
(hytest-with-error "1-8" 2.7590 (cuberoot 21))

;1-11
(hytest "1-11" 4 (f-recur 3))

;1-12
(hytest "1-12" 1 (pascal 1 1))
(hytest "1-12" 6 (pascal 5 3))

;1-16
(hytest "1-16" 8 (expo 2 3))
(hytest "1-16" 2048 (expo 2 11))

;1-17
(hytest "1-17" 30 (fast-expt 10 3))
(hytest "1-17" 80 (fast-expt 20 4))

;1-18
(hytest "1-18" 30 (fast-expt-iter 10 3))
(hytest "1-18" 80 (fast-expt-iter 20 4))

;1-19
(hytest "1-19" 40.0 (fib 11))

;1-21
(hytest "1-21" 7 (smallest-divisor 19999))
(hytest "1-21" 1999 (smallest-divisor 1999))
(hytest "1-21" 199 (smallest-divisor 199))

;1-22
;(search-for-primes 1 100000 (current-time))

;1-30
(hytest "1-30" 3025 (sum2 cube 1 inc 10))

;1-31
(hytest "1-31" 24 (factorial 4))
(hytest-with-error "1-31" 3.1415 (pi-prod 2000))
(hytest "1-31" 24 (product2 donothing 1 inc 4))

;1-32
(hytest "1-32" 3025 (sum3 cube 1 inc 10))
(hytest "1-32" 24 (product3 donothing 1 inc 4))

;1-33
(hytest "1-33" 88 (prime-squares 1 10))
(hytest "1-33" 14 (prime-squares 1 4))
(hytest "1-33" 100 (product-rel-prime 10))
(hytest "1-33" 13 (product-rel-prime 13))

;1-35
(hytest "1-35" 1.6180327868852458 golden-ratio)

;1-37
(hytest-with-error "1-37" (/ 1 golden-ratio) (cont-frac (fn [x] 1.0) (fn [x] 1.0) 10))
(hytest-with-error "1-37" (/ 1 golden-ratio) (cont-frac (fn [x] 1.0) (fn [x] 1.0) 100))
(hytest-with-error "1-37" (cont-frac (fn [x] 1.0) (fn [x] 1.0) 10) (cont-frac-recur (fn [x] 1.0) (fn [x] 1.0) 10))

;1-38
(hytest-with-error "1-38" 2.7182 (+ 2 (cont-frac n-euler d-euler 100)))

;1-39
(hytest-with-error "1-39" 1.0 (tan-cf (/ 3.141592 4) 100))

;1-40
(hytest-with-error "1-40" 1.0 (newtons-method (cubic -1 1 -1) 1))

;1-41
(hytest "1-41" 21 (((doagain (doagain doagain)) inc) 5))

;1-42
(hytest "1-42" 49 ((compose square inc) 6))

;1-43
(hytest "1-43" 625 ((repeated square 2) 5))
