(ns sicp-clj.ch1.hd)

; 1-27, 28 패스.

; 1-29
(defn cube[x] (* x x x))

(defn sum[term a next b]
	(if (> a b) 0
		(+ (term a) (sum term (next a) next b)))
)

(defn sub-cubes[a b]
	(sum cube a inc b))

(defn integral [f a b dx]
  (defn add-dx [x]
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))

(defn simpson-integral[f a b n]
	(let [h (/ (- b a) n)]
		(defn simp-term[k]
			(* (f (+ a (* k h)))
				(cond
					(or (zero? k) (= n k)) 1
					(odd? k) 4
					:else 2))		
		)
		(/ (* h (sum simp-term 0 inc n)) 3)
	)
)

(prn (integral cube 0 1 100)) ; => 0
(prn (integral cube 0 1 1000)) ; => 0
(prn (simpson-integral cube 0 1 100)) ; => 1/4
(prn (simpson-integral cube 0 1 1000)) ; => 1/4