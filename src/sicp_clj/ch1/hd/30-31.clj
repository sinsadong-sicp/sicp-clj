(ns sicp-clj.ch1.hd)

; 1-30

; (defn sum[term a next b]
; 	(if (> a b) 0
; 		(+ (term a) (sum term (next a) next b)))
; )

(defn sum-iter[term a next b]
	(defn iter[a result]
		(if (> a b) result
			(iter (next a) (+ (term a) result)))
	)
	(iter a 0)
)

; 1-31

(defn mul[term a next b]
	(if (> a b) 1
		(* (term a) (mul term (next a) next b)))
)

(defn mul-iter[term a next b]
	(defn iter[a result]
		(if (> a b) result
			(iter (next a) (* (term a) result)))
	)
	(iter a 1)
)

;k= 1   2   3   4   5   6   7

;   1   2 * 4 * 4 * 6 * 6 * 8 ...
;   - * ----------------------
;   1   3 * 3 * 5 * 5 * 7 * 7 ...

; 주어진 식은 위와 같이 1/1을 가장 왼쪽에 놓으면, 
; k = 1~n일 때
; 분모: (odd? k) k, else (inc k)
; 분자: (= k 1) 1, (odd? k) (inc k), else k
; 이렇게 상각할 수 있다.
; 그러니 next는 그냥 inc로 놓고, term을 위와 같이 정의하면 된다.

(defn get-pi-by-mul[n which-mul]
	(defn pi-term[k]
		(cond 
			(= k 1) 1
			(odd? k) (/ (inc k) k)
			:else (/ k (inc k)))
	)
	(double (* 4 (which-mul pi-term 1 inc n)))
)

(prn (get-pi-by-mul 10 mul)) ; => 3.002175954556907
(prn (get-pi-by-mul 10 mul-iter)) ; => 3.002175954556907
(prn (get-pi-by-mul 1000 mul)) ; => 3.140023818600597
(prn (get-pi-by-mul 1000 mul-iter)) ; => 3.140023818600597