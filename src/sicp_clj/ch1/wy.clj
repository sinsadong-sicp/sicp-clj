(ns sicp-clj.wy.ch1)

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

(ns sicp.ch-1-2)

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
  (if (n < 3)
    n
    (+ (f-recur (- n 1))
       (* 2 (f-recur (- n 2)))
       (* 3 (f-recur (- n 3))))))

(defn f-iter [n]
  )
