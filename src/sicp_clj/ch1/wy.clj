(ns sicp-clj.ch1.wy
  (:use [clojure.contrib.generic.math-functions :only [abs]]))

; 1-3
(defn sum-square-of-larger-two [a b c]
  (defn square [x] (* x x))
  (cond
    (and (<= c a) (<= c b)) (+ (square a) (square b))
    (and (<= b a) (<= b c)) (+ (square a) (square c))
    (and (<= a b) (<= a c)) (+ (square b) (square c))))

; 1-5
; applicative order keeps evaluating (p) to (p), and never terminates.
; normal order skips evaluating (p), returning 0 immediately.

; 1-6
; infinite loop. sqrt-iter recursively evaluates itself since new-if is an expression.

; 1-7
(defn average [x y]
  (/ (+ x y) 2))

(defn square [x]
  (* x x))

(defn sqrt [x]
  (defn improve [guess]
    (average guess (/ x guess)))
  ; (defn good-enough? [guess]
  ;   (< (Math/abs (- (square guess) x)) 0.001))
  (defn good-enough? [guess]
    (< (Math/abs (/ (- (improve guess) guess) guess)) 0.001))
  (defn sqrt-iter [guess]
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; 1-8
(defn cube [x]
  (* x x x))

(defn cbrt [x]
  (defn good-enough? [guess]
    (< (Math/abs (- (cube guess) x)) 0.001))
  (defn improve [guess]
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (defn cbrt-iter [guess]
    (if (good-enough? guess)
      guess
      (cbrt-iter (improve guess))))
  (cbrt-iter 1.0))

; 1-9
; first one's process is recursive as it expands like (inc (inc (inc ...))).
; second one's process is iterative as `+` procedure calls itself as a top expression.

; 1-10
; (A 0 n)
; => (* 2 n)

; (A 1 n)
; => (A 0 (A 1 (- n 1)))
; => (* 2 (A 1 (- n 1)))
; => (* 2 (* 2 (* 2 ... 2)))
; => (Math/pow 2 n)

; (A 2 n)
; => (A 1 (A 2 (- n 1)))
; => (Math/pow 2 (A 2 (- n 1)))
; => (Math/pow 2 (Math/pow 2 (Math/pow 2 ... 2)))
; => 2^2^2^... (n times)

; 1-11
(defn f-recur [n]
  (if (< n 3)
    n
    (+ (f-recur (- n 1))
       (* 2 (f-recur (- n 2)))
       (* 3 (f-recur (- n 3))))))

(defn f-iter [n]
  (defn iter [a b c x]
    (if (< x 3)
      a
      (iter (+ a (* 2 b) (* 3 c)) a b (dec x))))
  (iter 2 1 0 n))

; 1-12

(defn pascal [row col]
  (if (or (zero? col) (= col row))
    1
    (+ (pascal (dec row) col) (pascal (dec row) (dec col)))))

; 1-16

(defn fast-expt-iter [b n]
  (defn iter [acc prod c]
    (cond
      (zero? c) acc
      (even? c) (iter acc (square prod) (/ c 2))
      :else (iter (* acc prod) prod (dec c))))
  (iter 1 b n))

; 1-17
(defn do-double [x]
  (* 2 x))

(defn do-halve [x]
  (/ x 2))

(defn fast-mult [a b]
  (cond
    (zero? b) 0
    (even? b) (fast-mult (do-double a) (do-halve b))
    :else (+ a (fast-mult a (dec b)))))

; 1-18

(defn fast-mult-iter [a b]
  (defn iter [acc x y]
    (cond
      (zero? y) acc
      (even? y) (iter acc (do-double x) (do-halve y))
      :else (iter (+ acc x) x (dec y))))
  (iter 0 a b))

; 1-19

; Tpq(a, b)
;   = (bq + aq + ap, bp + aq)
; Tpq(Tpq(a, b))
;   = (bpq + aqq + bqq + aqq + apq + bpq + apq + app, bpp + apq + bqq + aqq + apq)
;   = (b(2pq + qq) + a(2qq + 2pq + pp), b(pp + qq) + a(2pq + qq))
;   = (bq' + aq' + ap', bp' + aq'), where p' = pp + qq, q' = 2pq + qq

(defn fib [n]
  (defn fib-iter [a b p q c]
    (cond
      (zero? c) b
      (even? c) (fib-iter a
                          b
                          (+ (square p) (square q))
                          (+ (* 2 p q) (square q))
                          (/ c 2))
      :else (fib-iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (dec c))))
  (fib-iter 1 0 0 1 n))

; 1-21

(defn smallest-divisor [n]
  (defn divides? [a b]
    (zero? (rem b a)))
  (defn find-divisor [n test-divisor]
    (cond
      (> (square test-divisor) n) n
      (divides? test-divisor n) test-divisor
      :else (find-divisor n (inc test-divisor))))
  (find-divisor n 2))

; (smallest-divisor 199) => 199
; (smallest-divisor 1999) => 1999
; (smallest-divisor 19999) => 7

; 1-29

(defn sum [term a advance b]
  (if (> a b)
    0
    (+ (term a) (sum term (advance a) advance b))))

(defn integral [f a b dx]
  (defn add-dx [x]
    (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))

(defn simpson-integral [f a b n]
  (let [h (/ (- b a) n)]
    (defn c [k]
      (cond
        (or (zero? k) (= n k)) 1
        (odd? k) 4
        :else 2))
    (defn g [k]
      (* (c k) (f (+ a (* k h)))))
    (* (/ h 3.0) (sum g 0 inc n))))

; 1-30

(defn sum-iter [term a advance b]
  (defn iter [a acc]
    (if (> a b)
      acc
      (iter (advance a) (+ acc (term a)))))
  (iter a 0))

; 1-31

(defn product-recur [term a advance b]
  (if (> a b)
    1
    (* (term a) (product-recur term (advance a) advance b))))

(defn product-iter [term a advance b]
  (defn iter [a acc]
    (if (> a b)
      acc
      (iter (advance a) (* acc (term a)))))
  (iter a 1))

(defn approximate-pi [n]
  (let [product product-recur]
    (defn term [k]
      (let [n (inc (* 2 k))]
        (/ (* (dec n) (inc n)) (square n))))
    (* 4.0 (product term 1 inc n))))

(defn factorial [n]
  (let [product product-recur]
    (product identity 1 inc n)))

; 1-32

(defn accumulate-recur [combiner null-value term a advance b]
  (if (> a b)
    null-value
    (combiner (term a) (accumulate-recur combiner null-value term (advance a) advance b))))

(defn accumulate-iter [combiner null-value term a advance b]
  (defn iter [a acc]
    (if (> a b)
      acc
      (iter (advance a) (combiner acc (term a)))))
  (iter a null-value))

(defn sum-using-accumulate [term a advance b]
  (let [accumulate accumulate-recur]
    (accumulate + 0 term a advance b)))

(defn product-using-accumulate [term a advance b]
  (let [accumulate accumulate-recur]
    (accumulate * 1 term a advance b)))

; 1-33

(defn filtered-accumulate [pred combiner null-value term a advance b]
  (defn iter [a acc]
    (if (> a b)
      acc
      (if (pred a)
        (iter (advance a) (combiner acc (term a)))
        (iter (advance a) acc))))
  (iter a null-value))

(defn prime? [n]
  (and (> n 1) (= n (smallest-divisor n))))

(defn sum-square-of-primes [a b]
  (filtered-accumulate prime? + 0 square a inc b))

(defn gcd [a b]
  (if (zero? b)
      a
      (gcd b (rem a b))))

(defn prod-coprimes [n]
  (defn coprime-to-n? [i]
    (= 1 (gcd i n)))
  (filtered-accumulate coprime-to-n? * 1 identity 1 inc n))

; 1-34
; (f f) => (f 2) => (2 2) => 2 is not a function, hence an exception is thrown

; 1-35
; x^2 = x + 1 => x = 1 + 1/x

(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (defn close-enough? [v u]
    (< (abs (- v u)) tolerance))
  (defn check [guess]
    (let [next-guess (f guess)]
      (if (close-enough? guess next-guess)
        next-guess
        (check next-guess))))
  (check first-guess))

(def golden-ratio
  (fixed-point (fn [x] (inc (/ 1 x))) 1.0))

; 1-37

(defn cont-frac-recur [n d k]
  (defn term [i]
    (if (> i k)
      0
      (/ (n i) (+ (d i) (term (inc i))))))
  (term 1))

(defn cont-frac-iter [n d k]
  (defn term [i acc]
    (if (> i k)
      acc
      (term (inc i) (/ (n i) (+ (d i) acc)))))
  (term 1 0))

; 1-38

(defn approximate-e [k]
  (defn n [i] 1.0)
  (defn d [i]
    (if (= (mod i 3) 2)
      (* 2.0 (/ (inc i) 3))
      1.0))
  (let [cont-frac cont-frac-recur]
    (+ 2 (cont-frac n d k))))

; 1-39

(defn tan-cf [x k]
  (defn n [i] (- (square x)))
  (defn d [i]
    (dec (* 2 i)))
  (let [cont-frac cont-frac-recur]
    (- (/ (cont-frac n d k) x))))
