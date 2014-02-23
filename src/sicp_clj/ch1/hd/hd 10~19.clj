(ns sicp-clj.ch1.hd)

; 1-10
(defn acnn [x y]
	(cond
		(= y 0) 0
		(= x 0) (* 2 y)
		(= y 1) 2
		:else (acnn (dec x) (acnn x (dec y))))
	)

;a 1 10 = 1024

;a 2 4 = 65536
;a 1 (a 2 3)
;a 1 (a 1 (a 2 2))
;a 1 (a 1 (a 1 (a 2 1)))
;a 1 (a 1 (a 1 2))
;a 1 (a 1 4)
;a 1 16
;65536

;a 3 3 = 65536
;a 2 (a 3 2)
;a 2 (a 2 (a 3 1))
;a 2 (a 2 2)
;a 2 (a 1 (a 2 1))
;a 2 (a 1 2)
;a 2 4
;65536

; (defn acnn-f[n] (acnn 0 n)) => 2n
; (defn acnn-g[n] (acnn 1 n)) => 2^n
; (defn acnn-h[n] (acnn 2 n)) => 2^2^ ... (n번) => 수학적으로 어떻게 표시? 
; 양의 정수 n에 대해 h(n) = 2 when n is 1, 2^{h(n-1)} others 이렇게 하면 되나.

; 1-11
(defn fib[n]
	(defn iter[a b count]
		(if (= count 0) 
			b 
			(iter (+ a b) a (dec count))))
	(iter 1 0 n))

(defn ex11-rec[n]
	(cond 
		(< n 3) n
		:else (+ 
			(ex11-rec (- n 1))
			(* 2 (ex11-rec (- n 2)))
			(* 3 (ex11-rec (- n 3))))
		))

(defn ex11-iter[n]
	(defn iter[a b c count]
		(if (= count 0)
			c
			(iter (+ a (* 2 b) (* 3 c)) a b (dec count))))
	(iter 2 1 0 n))

; 1-12
(defn pascal-tri[n]	
	(defn pascal-val[level idx]
		(cond			
			(= idx 0) 1
			(= idx level) 1
			:else (+ (nth (pascal-tri (dec level)) (dec idx)) (nth (pascal-tri (dec level)) idx))
			))
	(into [] (for [x (range 0 (inc n))] (pascal-val n x))))

; 1-13 => pass

; 1-14 => pass

; 1-15
(defn pow[n, x]
	(if (= x 0) 1 (reduce * (take x (repeat n)))))
; a. 3^4=81, 3^5=243. 3의 x승이 121.5을 넘기면 종료하는 것이니까 5번.
; b. (sine a)는 a를 0.1로 나눈 값이 3의 x승을 넘길 때까지 계산. 즉 ceiling(log (10a) / (log 3)) 번 한다. 자람 차수는 (log a).

; 1-16
(defn expt[b n]
	(defn expt-iter[counter product]
		(if (= counter 0)
			product
			(expt-iter (dec counter) (* b product)
		)))
	(expt-iter n 1))

(defn fast-expt[b n]
	(cond 
		(= n 0) 1
		(even? n) (square (fast-expt b (/ n 2)))
		:else (* b (fast-expt b (dec n)))
	))

(defn fast-expt-iter[b n]
	(defn iter[counter product]
		(cond 
			(= counter 0) product
			(even? counter) (square (iter (/ counter 2) product))
			:else (iter (dec counter) (* b product))
		))
	(iter n 1))

; 1-17
(defn multiply [a b]
	(defn double_ [x] (+ x x))
	(defn halve [x] (/ x 2))
	(cond 
		(= b 0) 0
		(even? b) (multiply (double_ a) (halve b))
		:else (+ a (multiply a (dec b)))))

; 1-18
(defn multiply-iter [a b]
	(defn double_ [x] (+ x x))
	(defn halve [x] (/ x 2))
	(defn iter[counter product]
		(cond
			(= counter 0) 0
			(= counter 1) product
			(even? counter) (iter (halve counter) (double_ product))
			:else (+ b (iter (dec counter) product))
		))
	(iter a b))

; 1-19
; p'와 q'가 어디서 튀어나온건가??
; a' = bq + aq + ap , b' = bp + aq
; a'' = a(2q^2 + 2pq + p^2) + b(q^2 + 2pq)
;	  = aq' + ap' + bq'
; q' = q^2 + 2pq
; q' + p' = 2q^2 + 2pq + p^2
; p' = p^2 + q^2