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