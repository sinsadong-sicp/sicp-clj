(ns sicp-clj.ch2.wy
  (:use [clojure.contrib.math :only [abs expt gcd sqrt]])
  (:use [clojure.contrib.generic.math-functions :only [sgn]]))

; 2-1

(defn make-rational [n d]
  (let [g (gcd n d)
        s (* (sgn n) (sgn d))]
    (vector
      (* s (/ (abs n) g))
      (/ (abs d) g))))

; 2-2

(defn make-point [x y]
  (vector x y))
(defn x-point [p]
  (first p))
(defn y-point [p]
  (second p))

(defn make-segment [p q]
  (vector p q))
(defn start-of-segment [s]
  (first s))
(defn end-of-segment [s]
  (second s))

(defn midpoint-of-segment [s]
  (defn average [x y]
    (/ (+ x y) 2))
  (let [p (start-of-segment s)
        q (end-of-segment s)
        x (average (x-point p) (x-point q))
        y (average (y-point p) (y-point q))]
    (make-point x y)))

; 2-3

(defn length-of-segment [s]
  (let [p (start-of-segment s)
        q (end-of-segment s)]
    (sqrt
      (+
        (expt (- (x-point p) (x-point q)) 2)
        (expt (- (y-point p) (y-point q)) 2)))))

; bottom left point + top right point
(defn make-rectangle [p q]
  (vector p q))
(defn length-of-rectangle [r]
  (let [p (first r)
        q (second r)]
    (length-of-segment
      (make-segment
        p
        (make-point (x-point q) (y-point p))))))
(defn width-of-rectangle [r]
  (let [p (first r)
        q (second r)]
    (length-of-segment
      (make-segment
        p
        (make-point (x-point p) (y-point q))))))

(defn perimeter-of-rectangle [r]
  (* 2 (+ (length-of-rectangle r) (width-of-rectangle r))))

(defn area-of-rectangle [r]
  (* (length-of-rectangle r) (width-of-rectangle r)))

; 2-4
; (car (cons x y))
; => ((cons x y) (fn [p q] p))
; => ((fn [m] (m x y)) (fn [p q] p))
; => ((fn [p q] p) x y)
; => x

(defn cdr [z]
  (z (fn [p q] q)))

; 2-5

(defn my-cons [a b]
  (* (expt 2 a) (expt 3 b)))

(defn my-car [n]
  (if (odd? n)
    0
    (inc (my-car (/ n 2)))))

(defn my-cdr [n]
  (if (not (zero? (rem n 3)))
    0
    (inc (my-cdr (/ n 3)))))

; 2-6

(def zero
  (fn [f]
    (fn [x]
      x)))

(defn add-1 [n]
  (fn [f]
    (fn [x]
      (f ((n f) x)))))

(def one
  (fn [f]
    (fn [x]
      (f x))))

(def two
  (fn [f]
    (fn [x]
      (f (f x)))))

(defn add [m n]
  (fn [f] (comp (m f) (n f))))

; 2-7

(defn make-interval [x y]
  (vector x y))
(defn upper-bound [interval]
  (first interval))
(defn lower-bound [interval]
  (second interval))

(defn add-interval [x y]
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(defn multiply-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (lower-bound y))]
    (make-interval (max p1 p2 p3 p4) (min p1 p2 p3 p4))))

(defn divide-interval [x y]
  (let [inverse-of-y (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))]
    (multiply-interval x inverse-of-y)))

; 2-8

(defn subtract-interval [x y]
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))

; 2-17

(def car first)
(def cdr rest)

(defn last-pair [lst]
  (if (empty? (cdr lst))
    (list (car lst))
    (last-pair (cdr lst))))

; 2-18

(defn append [xs ys]
  (if (empty? xs)
    ys
    (cons (car xs) (append (cdr xs) ys))))

(defn rev [lst]
  (if (empty? (cdr lst))
    (list (car lst))
    (append (rev (cdr lst)) (list (car lst)))))

; 2-20

(defn same-parity [& lst]
  (let [same-parity? (if (odd? (car lst)) odd? even?)]
    (defn iter [xs ys]
      (if (empty? ys)
        xs
        (if (same-parity? (car ys))
          (iter (append xs (list (car ys))) (cdr ys))
          (iter xs (cdr ys)))))
    (iter (list (car lst)) (cdr lst))))

; 2-21

(defn square-list-1 [items]
  (defn iter [xs ys]
    (if (empty? ys)
      xs
      (iter (append xs (list (expt (first ys) 2))) (rest ys))))
  (iter nil items))

(defn square-list-2 [items]
  (map (fn [x] (expt x 2)) items))

; 2-22
; the last item in 'items' is the last to be cons-ed to the front of 'answer' list.
; interchanging arguments makes the first argument to be of list type.

; 2-23

(defn foreach [f lst]
  (if (empty? lst)
    true
    (do
      (f (car lst))
      (foreach f (cdr lst)))))

; 2-25
; (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))) ; => 7
; (car (car (list (list 7)))) ; => 7
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))) ; => 7

; 2-26
; (def x (list 1 2 3))
; (def y (list 4 5 6))
; (append x y) ; => (1 2 3 4 5 6)
; (cons x y) ; => ((1 2 3) 4 5 6)
; (list x y) ; => ((1 2 3) (4 5 6))
