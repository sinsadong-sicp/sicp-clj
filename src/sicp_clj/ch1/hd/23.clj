(ns sicp-clj.ch1.hd)

; 1-23
(defn fast-smallest-divisor[n]
	(defn next-divisor[x]
		(if (= x 2) 3 (+ x 2)))
	(defn divides? [a b]
      (= (rem b a) 0))
	(defn find-divisor [test-divisor]
	    (cond 
	    	(> (square test-divisor) n) n
		    (divides? test-divisor n) test-divisor
		    :else (find-divisor (next-divisor test-divisor))))
    (find-divisor 2))

(defn prime?[n]
  (= n (fast-smallest-divisor n))
;  (= n (smallest-divisor n))
 )

; 변동이 심해서 평균내기가 어렵다. 
(prn (search-for-prime 1000 1100)) ; 0.01 -> 0.025
(prn (search-for-prime 10000 10100)) ; 0.03 -> 0.05
(prn (search-for-prime 100000 100100)) ; 0.09 -> 0.053~0.066
(prn (search-for-prime 1000000 1000100)) ; 0.27 -> 0.16~0.19

; 수가 작을 때는 빨라지지 않는다. inc 함수가 아닌 내가 정의한 next-divisor를 부르는 것의 cost가 있는 것 같다.
; 변동이 심해 진짜 비율을 짐작하기 어렵다.