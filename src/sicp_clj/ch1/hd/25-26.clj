(ns sicp-clj.ch1.hd)

; 1-25 (1-24는 패스)
; 아래는 참고
; (defn expmod[base exp m]
;   (cond
;     (zero? exp) 1
;     (even? exp) (rem (square (expmod base (/ exp 2) m)) m)
;     :else (rem (* base (expmod base (dec exp) m)) m)
;   ))

; (defn fast-expt[b n]
;   (cond 
;     (zero? n) 1
;     (even? n) (square (fast-expt b (/ n 2)))
;     :else (* b (fast-expt b (dec n)))
;   ))

(defn expmod-1-25[base exp m]
	(rem (fast-expt base exp) m))

; fast-expt는 너무 큰 수를 계산할 때 integer overflow가 난다. (예를 들어 3^40)
; 소수 계산을 할 때는 매우 큰 수를 다뤄야 하는데 overflow가 나면 안 된다.
; 반면 기존의 expmod는 base^exp를 바로 계산하는 게 아니라 수를 줄여나가면서 연산하므로 괜찮다.

; 1-26
; 말 그대로, exp만큼 expmod가 호출되니까 O(n)이다. 반면 square를 쓰면 expmod 연산이 O(log2 n)이 된다.