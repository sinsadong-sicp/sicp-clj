(ns sicp-clj.ch1.hy)

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

;| p+q  q | | a | = | ap+aq bq |
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
