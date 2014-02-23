(ns sicp-clj.ch1.hd)

; 1-20
(defn gcd[a b]
	(if (= b 0) 
		a
		(gcd b (rem a b)))
	)
; substitution - normal order: 안끝나는 것 아닌가?
; (gcd 206 40)
; (gcd 40 (rem 206 40))
; (gcd (rem 206 40) (rem 40 (rem 206 40)))
; (gcd (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40))))

; applicative order: 총 4번.
; (gcd 206 40)
; (gcd 40 (rem 206 40)) = (gcd 40 6)
; (gcd 6 (rem 40 6)) = (gcd 6 4)
; (gcd 4 (rem 4 2)) = (gcd 4 2)
; (gcd 2 (rem 2 0)) = (gcd 2 0)
; 2

; 1.2.6 연습: 소수 찾기
(defn smallest-divisor [n]
	(defn divides? [a b]
  		(= (rem b a) 0))
	(defn find-divisor [test-divisor]
		(cond 
			(> (square test-divisor) n) n
			(divides? test-divisor n) test-divisor
			:else (find-divisor (inc test-divisor))))
  	(find-divisor 2))

(defn prime?[n]
	(= n (smallest-divisor n)))

(defn expmod[base exp m]
	(cond
		(= exp 0) 1
		(even? exp) (rem (square (expmod base (/ exp 2) m)) m)
		:else (rem (* base (expmod base (dec exp) m)) m)
	))

(defn fermat-test[n]
	(defn try-it[a]
		(= expmod a n n) a)
	(try-it (inc (rand (dec n)))))

(defn fast-prime? [n times]
	(cond
		(= times 0) true
		(fermat-test n) (fast-prime? n (dec times))
		:else false
	))  	

; 1-21
; 199 -> 199, 1999 -> 1999, 19999 -> 7

; 1-22
(defn timed-prime-test[n]
	(def elapsed (time (prime? n)))
	(newline)
	(print n)
	(if (prime? n) (print elapsed))
	)

