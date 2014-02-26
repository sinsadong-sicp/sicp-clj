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

