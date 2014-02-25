(ns sicp-clj.ch1.wy)

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

