(ns sicp-clj.ch1.hd
	(:use clojure.contrib.generic.math-functions))

; 1-34
(defn f[g] (g 2))
; (f f) 를 하면 어떻게 되는가? Class cast exception이 나오는군. Long을 IFn으로 바꿀 수 없다.
; 실행하면 이렇게 되겠지. (f f) -> (f 2) -> (2 2)
; 그러니까 2를 함수로 바꿀 수 없다는 뜻. 실제로 (2 2)해서 나오는 메시지랑 거의 같다.

; 1-35
(defn close-enough?[x y]
	(def tolerance 0.00001)
	(< (abs (- x y)) tolerance))

(defn average[x y]
  (/ (+ x y) 2))

(defn search[f neg-p pos-p]	
	(let [mid-p (average neg-p pos-p)]
		(if (close-enough? neg-p pos-p) mid-p
			(let [test-val (f mid-p)]
				(cond 
					(pos? test-val) (search f neg-p mid-p)
					(neg? test-val) (search f mid-p pos-p)
					:else mid-p
)))))

(defn half-interval-method[f a b]
	(let [a-val (f a) b-val (f b)]
		(cond
			(and (neg? a-val) (pos? b-val)) (search f a b)
			(and (neg? b-val) (pos? a-val)) (search f b a)
			:else (prn "Values are not of opposite sign" a b)
)))

(defn fixed-point[f first-guess]
	(defn try-guess[guess]
		(let [next-val (f guess)]
			(if (close-enough? guess next-val)
				next-val (try-guess next-val))))
	(try-guess first-guess))

(def golden-ratio ; 1.6180327868852458이 나온다.
	(fixed-point (fn[x] (inc (/ 1 x))) 1.0))