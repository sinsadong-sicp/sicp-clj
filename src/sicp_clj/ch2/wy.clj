(ns sicp-clj.ch2.wy
  (:use [clojure.contrib.math :only [abs expt gcd sqrt]])
  (:use [clojure.contrib.generic.math-functions :only [sgn]]))

; 2-1

(defn make-rational [n d]
  (let [g (gcd n d)
        s (* (sgn n) (sgn d))]
    [(* s (/ (abs n) g)) (/ (abs d) g)]))

; 2-2

(defn make-point [x y]
  [x y])
(defn x-point [p]
  (first p))
(defn y-point [p]
  (second p))

(defn make-segment [p q]
  [p q])
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
  [p q])
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
