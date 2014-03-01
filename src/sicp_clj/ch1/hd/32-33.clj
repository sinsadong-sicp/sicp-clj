(ns sicp-clj.ch1.hd)

; 1-32-a

(defn accumulate[combiner null-value term a next b]
	(if (> a b) null-value
		(combiner (term a) (accumulate combiner null-value term (next a) next b))))

(defn sum-acc[term a next b]
	(defn plus[a b](+ a b))
	(accumulate plus 0 term a next b))

(defn mul-acc[term a next b]
	(defn multiply[a b](* a b))
	(accumulate multiply 1 term a next b))

; 1-32-b
(defn accumulate-iter[combiner null-value term a next b]
	(defn iter[a result]
		(if (> a b) result
			(iter (next a) (combiner (term a) result))))
	(iter a null-value))

(defn sum-acc-iter[term a next b]
	(defn plus[a b](+ a b))
	(accumulate-iter plus 0 term a next b))

(defn mul-acc-iter[term a next b]
	(defn multiply[a b](* a b))
	(accumulate-iter multiply 1 term a next b))

(prn (sum-acc identity 1 inc 10)) ; => 55
(prn (sum-acc-iter identity 1 inc 10)) ; => 55
(prn (mul-acc identity 1 inc 10)) ; => 3628800
(prn (mul-acc-iter identity 1 inc 10)) ; => 3628800

; 1-33
(defn fast-smallest-divisor[n]
	(defn next-divisor[x]
		(if (= x 2) 3 (+ x 2)))
	(defn divides? [a b]
      (zero? (rem b a)))
	(defn square[a] (* a a))
	(defn find-divisor [test-divisor]
	    (cond 
	    	(> (square test-divisor) n) n
		    (divides? test-divisor n) test-divisor
		    :else (find-divisor (next-divisor test-divisor))))
    (find-divisor 2))

(defn prime?[n]
	(if (= n 1) false
		(= n (fast-smallest-divisor n))))

(defn filtered-accumulate[combiner null-value term a next b filter]	
	(defn iter[a result]
		(cond 
			(> a b) result
			(filter a) (iter (next a) (combiner (term a) result))
			:else (iter (next a) result)))
	(iter a null-value))

(defn sum-of-sqares-of-primes[a b]
	(defn plus[a b](+ a b))
	(filtered-accumulate plus 0 square a inc b prime?))

(defn prod-of-pos-ints-less-and-rel-prime-to-n[n]
	(defn multiply[a b](* a b))	
	(defn relative-prime?[x]
		(defn gcd[a b]
		  (if (zero? b) a
		    (gcd b (rem a b))))
		(= (gcd x n) 1))
	(filtered-accumulate multiply 1 identity 1 inc n relative-prime?)
)

(prn (sum-of-sqares-of-primes 1 10)) ; => 87
(prn (prod-of-pos-ints-less-and-rel-prime-to-n 10)) ; => 189