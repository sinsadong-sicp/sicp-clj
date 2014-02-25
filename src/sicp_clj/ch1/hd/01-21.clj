(ns sicp-clj.ch1.hd)

; 1-3
(defn sum-of-squares-of-two-biggest[a b c]
  (defn square [x] (* x x))
  (defn sum-of-squares [[a b]] (+ (square a) (square b)))
  (sum-of-squares (rest (sort [a b c]))))

; 1-5
; 문제를 클로저 식으로 쓰면 다음과 같다.
; (def p p)
; (defn test[x y] (if (= x 0) 0 y)) 
; 인자 우선: p를 p로 정의했으니 이걸 evaluate하다가 무한 루프에 빠진다.
; normal order: test를 if가 포함된 exp로 바꾼다. 그러면 첫 조건에 걸리므로 그냥 0.

; 1-6
; 그냥 if로 하면 잘 계산되고, new-if로 하면 call stack이 초과되었다고 나온다.
; 이유가 뭘지 깊이 생각 안해보고 솔루션을 찾아봤는데, 도움이 되긴 하지만 스스로 생각한 다음 찾는 게 좋겠다. 이제는 다 풀고 찾아보자.
; 아무튼 답은 
; 	special form인 if는 한 번에 하나만 evaluate하게 되어있는데(인자 우선임에도 불구하고) 
;	new-if는 평범한 프로시저이기 때문에 모든 sub-expressions를 먼저 evaluate한다. 
;	따라서 if 는 언젠가 종료하지만 new-if는 무한루프에 빠진다.

; 1-7
(defn square[x] (* x x))

(defn average[x y]
  (/ (+ x y) 2))

(defn abs[x]
  (if (> x 0) x (- x))) 

(defn sqrt[x]
  (defn good-enough?[guess]
    (< (abs (- (square guess) x)) 0.001))
  (defn improve[guess]
    (average guess (/ x guess)))
  (defn sqrt-iter[guess]
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(defn delta-with-x[what x]
  (abs (- (square what) x)))

(defn improve[guess x]
  (average guess (/ x guess)))

(defn good-enough?[guess x]
  (<= (delta-with-x guess x) 
      (delta-with-x (improve guess x) x)))  

(defn sqrt-new[x]
  (defn sqrt-iter[guess]
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x))))
  (sqrt-iter 1.0))

; --- 2주차 (~2월 9일) ---
(defn factorial-pure[n] ;정상작동
	(if (= n 1) 1 (* n (factorial-pure (- n 1)))))

(defn factorial-iter[n] ;integer overflow 발생
	(defn iter[product counter]
		(if > counter n)
			product
			(iter (* counter product) (+ counter 1)))
	(iter 1 1))

(defn factorial-reduce[n]
	(reduce * (range 1 (inc n))))

; 1-9
; inc 먼저: resursive
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

; dec 먼저: interative
; (+ 4 5)
; (+ (dec 4) (inc 5))
; (+ 3 6)
; (+ (dec 3) (inc 6))
; (+ 2 7)
; (+ (dec 2) (inc 7))
; (+ 1 8)
; (+ (dec 1) (inc 8))
; (+ 0 9)
; 9

; 1-10
(defn acnn [x y]
  (cond
    (zero? y) 0
    (zero? x) (* 2 y)
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
    (if (zero? count) 
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
    (if (zero? count)
      c
      (iter (+ a (* 2 b) (* 3 c)) a b (dec count))))
  (iter 2 1 0 n))

; 1-12
(defn pascal-tri[n] 
  (defn pascal-val[level idx]
    (cond     
      (zero? idx) 1
      (= idx level) 1
      :else (+ (nth (pascal-tri (dec level)) (dec idx)) (nth (pascal-tri (dec level)) idx))
      ))
  (into [] (for [x (range 0 (inc n))] (pascal-val n x))))

; 1-13 => pass

; 1-14 => pass

; 1-15
(defn pow[n, x]
  (if (zero? x) 1 (reduce * (take x (repeat n)))))
; a. 3^4=81, 3^5=243. 3의 x승이 121.5을 넘기면 종료하는 것이니까 5번.
; b. (sine a)는 a를 0.1로 나눈 값이 3의 x승을 넘길 때까지 계산. 즉 ceiling(log (10a) / (log 3)) 번 한다. 자람 차수는 (log a).

; 1-16
(defn expt[b n]
  (defn expt-iter[counter product]
    (if (zero? counter)
      product
      (expt-iter (dec counter) (* b product)
    )))
  (expt-iter n 1))

(defn fast-expt[b n]
  (cond 
    (zero? n) 1
    (even? n) (square (fast-expt b (/ n 2)))
    :else (* b (fast-expt b (dec n)))
  ))

(defn fast-expt-iter[b n]
  (defn iter[counter product]
    (cond 
      (zero? counter) product
      (even? counter) (square (iter (/ counter 2) product))
      :else (iter (dec counter) (* b product))
    ))
  (iter n 1))

; 1-17
(defn multiply [a b]
  (defn double_ [x] (+ x x))
  (defn halve [x] (/ x 2))
  (cond 
    (zero? b) 0
    (even? b) (multiply (double_ a) (halve b))
    :else (+ a (multiply a (dec b)))))

; 1-18
(defn multiply-iter [a b]
  (defn double_ [x] (+ x x))
  (defn halve [x] (/ x 2))
  (defn iter[counter product]
    (cond
      (zero? counter) 0
      (= counter 1) product
      (even? counter) (iter (halve counter) (double_ product))
      :else (+ b (iter (dec counter) product))
    ))
  (iter a b))

; 1-19
; p'와 q'가 어디서 튀어나온건가??
; a' = bq + aq + ap , b' = bp + aq
; a'' = a(2q^2 + 2pq + p^2) + b(q^2 + 2pq)
;   = aq' + ap' + bq'
; q' = q^2 + 2pq
; q' + p' = 2q^2 + 2pq + p^2
; p' = p^2 + q^2

; 1-20
(defn gcd[a b]
  (if (zero? b) 
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
      (zero? (rem b a)))
  (defn find-divisor [test-divisor]
    (cond 
      (> (square test-divisor) n) n
      (divides? test-divisor n) test-divisor
      :else (find-divisor (inc test-divisor))))
    (find-divisor 2))

; (defn prime?[n]
;   (= n (smallest-divisor n)))

(defn expmod[base exp m]
  (cond
    (zero? exp) 1
    (even? exp) (rem (square (expmod base (/ exp 2) m)) m)
    :else (rem (* base (expmod base (dec exp) m)) m)
  ))

(defn fermat-test[n]
  (defn try-it[a]
    (= expmod a n n) a)
  (try-it (inc (rand (dec n)))))

(defn fast-prime? [n times]
  (cond
    (zero? times) true
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