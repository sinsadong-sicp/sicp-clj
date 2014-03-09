(ns sicp-clj.ch1.hy)

;1-3
(println (sum-of-squares-of-larger-two 1 2 3))
(println (sum-of-squares-of-larger-two 4 1 2))
(println (sum-of-squares-of-larger-two -1 -2 -3))

;1-7
(println (sqrt 16))
(println (sqrt 13))

;1-8
(println (cuberoot 27))
(println (cuberoot 21))

;1-11
(println (f-recur 3))

;1-12
(println (pascal 1 1))
(println (pascal 5 3))

;1-16
(println (expo 2 3))
(println (expo 2 11))

;1-17
(println (fast-expt 10 3))
(println (fast-expt 20 4))

;1-18
(println (fast-expt-iter 10 3))
(println (fast-expt-iter 20 4))

;1-19
(println (fib 11))

;1-21
(println (smallest-divisor 19999))
(println (smallest-divisor 1999))
(println (smallest-divisor 199))

;1-22
;(search-for-primes 1 100000 (current-time))

;1-30
(println (sum2 cube 1 inc 10))

;1-31
(println (factorial 4))
(println (pi-prod 10))
(println (product2 donothing 1 inc 4))
