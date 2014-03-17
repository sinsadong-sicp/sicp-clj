(ns sicp-clj.ch1.hy.hy)

(defn square [x]
  (* x x))
(defn cube [x]
  (* x (square x)))
(defn abs [x]
  (if (< x 0.0)
    (- 0.0 x)
    x))
(defn average [x y]
  (/ (+ x y) 2))
(defn halve [x]
  (/ x 2))
(defn mult2 [x]
  (* 2 x))


; 1-3
(defn sum-of-squares-of-larger-two [a b c]
  (if (and (< c a) (< c b))
    (+ (* a a) (* b b))
    (if (and (< b a) (< b c))
      (+ (* a a) (* c c))
      (+ (* b b) (* c c)))))

; 1-5
(comment
  normal order evaluation일 경우 무한루프
  applicative order evaluation일 경우 0 )

; 1-6
; 무한루프

; 1-7
(defn sqrt [x]
  (defn sq-iter [guess gbefore]
    (defn improve [b a]
      (average b (/ a b)))
    (defn isgood [est ori]
      (if (= est ori)
        (= 1 1)
        (< (abs (/ ori (- ori est))) 0.001)))
    (if (isgood guess gbefore)
      guess
      (sq-iter (improve guess x) guess)))
  (sq-iter 1.0 x))

; 1-8
(defn cuberoot [x]
  (defn cube-iter [guess]
    (defn improve [b a]
      (/ (+ (/ a (square b)) (* 2 b)) 3))
    (defn isgood [est ori]
      (< (abs (- (cube est) ori)) 0.1))
    (if (isgood guess x)
      guess
      (cube-iter (improve guess x))))
  (cube-iter 1.0))

; 1-11
(defn f-recur [n]
  (cond (< n 3) n
         :else (+ (+ (f-recur (- n 1))
                 (* 2 (f-recur (- n 2))))
                 (* 3 (f-recur (- n 3))))))

; 1-12
(defn pascal [n t]
  (cond (or (<= n 1) (<= t 1))  1
         (<= n t) 1
         :else (+ (pascal (- n 1) (- t 1)) (pascal (- n 1) t))))

; 1-16
(defn expo [b n]
  (defn expo-step [b n a]
    (cond
      (even? n) (expo-step (square b) (/ n 2) a)
      (= n 1)  (* a b)
      (= n 0)  a
      :else (expo-step b (- n 1) (* a b))))
  (expo-step b n 1))

; 1-17
(defn double2 [a]
  (+ a a))
(defn halve2 [a]
  (/ a 2))
(defn fast-expt [a b]
  (cond  (= b 0) 0
          (= b 1) a
          (even? b) (fast-expt (double2 a) (halve2 b))
          :else (+ a (fast-expt a (- b 1)))))

; 1-18
(defn fast-expt-iter [a b]
  (defn iter [x y sum]
    (cond
      (= y 0) sum
      (even? y) (iter (double2 x) (halve2 y) sum)
      :else (iter x (- y 1) (+ sum x))))
  (iter a b 0))

; 1-19

;| p+q  q | | a | = | ap+aq  bq |
;|  q   p | | b | = |   aq   bp |
;앞의 메트릭스  square하면
;| p^2+2pq+2q^2    q^2+2pq |
;|   q^2+2pq       p^2+q^2 |
;p' = p^2 + q^2
;q' = q^2 + 2pq


(defn fib [n]
  (defn fib-iter [a b p q cnt]
  (cond
    (= cnt 0) b
    (even? cnt) (fib-iter a b (+ (square p) (square q)) (+ (square q) (double (* p q))) (halve cnt))
    :else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- cnt 1))))
  (fib-iter 1 0 0 1 n))

; 1-21
(defn divides? [a b]
  (= (rem b a) 0))
(defn find-divisor [n test-divisor]
  (cond
    (> (square test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor n (+ test-divisor 1))))
(defn smallest-divisor [n]
  (find-divisor n 2))

; 1-22
(defn prime? [n]
  (= (smallest-divisor n) n))
(defn current-time []
  (System/currentTimeMillis))
(defn report-prime []
  (str " *** "))
(defn start-prime-test [n start-time]
  (cond (prime? n) (report-prime)))
(defn timed-prime-test [init-time n]
  (str "\n" n (start-prime-test n init-time) " elapsed time " (- (current-time) init-time)))
(defn search-for-primes [start end initial-time]
  (defn make-odd [s e]
    (cond
      (divides? 2 start) (range (inc start) end 2)
      :else (range start end 2)))
  (print (take-last 3 (filter (complement nil?) (map (partial timed-prime-test initial-time) (make-odd start end))))))

; 1-29
(defn sum [term a nxt b]
  (if (> a b)
    0
    (+ (term a) (sum term (nxt a) nxt b))))
(defn simpson [f a b n]
  (defn coeff [idx]
    (cond
      (= idx 0) 1
      (= idx n) 1
      (divides? 2 idx) 4
      :else 2))
  (let [h (/ (- b a) n)]
    (defn term [idx]
      (* (coeff idx) (f (+ a (* idx h)))))
    (* (/ h 3) (sum term 0 inc n)
  )))

; 1-30
(defn sum2 [term a nxt b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (nxt a) (+ (term a) result))))
  (iter a 0))

; 1-31
(defn donothing [y] y)
(defn product [term a nxt b]
  (if (> a b)
    1
    (* (term a) (product term (nxt a) nxt b))))
(defn product2 [term a nxt b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (nxt a) (* (term a) result))))
  (iter a 1))
(defn factorial [x]
  (product donothing 1 inc x))
(defn pi-prod [n]
  (defn num-term [x]
    (if (= x 1)
      2
      (* (inc (quot x 2)) 2)))
  (defn deno-term [x]
    (inc (mult2 (quot (inc x) 2))))
  (defn term [x]
    (/ (num-term x) (deno-term x)))
  (double (* 4 (product2 term 1 inc n))))

; 1-32
(defn accumulate [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(defn accumulate-iter [combiner null-value term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))
(defn product3 [term a next b]
  (accumulate * 1 term a next b))
(defn sum3 [term a next b]
  (accumulate + 0 term a next b))

;1-33
(defn filtered-accumulate [combiner null-value term a next b fil]
  (defn iter [a result]
    (if (> a b)
      result
      (if (fil a)
        (iter (next a) (combiner (term a) result))
        (iter (next a) result))))
  (iter a null-value)
)

(defn prime-squares [a b]
  (filtered-accumulate + 0 square a inc b prime?))

(defn product-rel-prime [n]
  (defn isdivisor? [a]
    (divides? a n))
  (filtered-accumulate * 1 identity 1 inc n isdivisor?))

;1-34
(defn f [g]
  (g 2))
; (f f) gives ClassCassException

;1-35
(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn trial [guess]
    (let [next (f guess)]
      (if (close-enough? guess next)
        next
        (trial next)
        )))
  (trial first-guess))

(def golden-ratio
  (fixed-point (fn [x] (inc (/ 1 x))) 1.0))

;1-37
(defn cont-frac [n d k]
  (defn iter [step result]
    (if (zero? step)
      result
      (iter (dec step) (/ (n step) (+ (d step) result)))))
  (iter k 0))

(defn cont-frac-recur [n d k]
  (if (zero? k)
    0
    (/ (n k) (+ (d k) (cont-frac-recur n d (dec k))))))

;1-38
(defn n-euler [k]
  1.0)
(defn d-euler [k]
  (cond
    (= k 1) 1.0
    (= k 2) 2.0
    (= (rem (- k 2) 3) 0) (double (double2 (inc (quot (- k 2) 3))))
    :else 1.0))

;1-39
(defn tan-cf [x k]
  (defn n [step]
    (if (= step 1)
      x
      (- (square x))))
  (defn d [step]
    (inc (double2 (dec step))))
  (cont-frac n d k))

;1-40
(def dx 0.00001)
(defn deriv [g]
  (fn [x]
    (/ (- (g (+ x dx)) (g x)) dx)))
(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))
(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))
(defn cubic [a b c]
  (fn [x]
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

;1-41
(defn doagain [g]
  (fn [x]
    (g (g x))))

;1-42
(defn compose [f g]
  (fn [x]
    (f (g x))))

;1-43
(defn repeated [f n]
  (if (= n 1)
    f
    (compose f (repeated f (dec n)))))

;1-44
(defn smooth [f]
  (fn [x]
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(defn n-fold-smooth [f n]
  (repeated f n))
