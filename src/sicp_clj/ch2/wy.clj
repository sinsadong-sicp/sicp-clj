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

(defn cons-int [a b]
  (* (expt 2 a) (expt 3 b)))

(defn car-int [n]
  (if (odd? n)
    0
    (inc (car-int (/ n 2)))))

(defn cdr-int [n]
  (if (not (zero? (rem n 3)))
    0
    (inc (cdr-int (/ n 3)))))

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

(defn last-pair [xs]
  (if (empty? (cdr xs))
    (list (car xs))
    (last-pair (cdr xs))))

; 2-18

(defn append [xs ys]
  (if (empty? xs)
    ys
    (cons (car xs) (append (cdr xs) ys))))

(defn rev [xs]
  (if (empty? xs)
    nil
    (append (rev (cdr xs)) (list (car xs)))))

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

(defn square-list-1 [lst]
  (defn iter [xs ys]
    (if (empty? ys)
      xs
      (iter
        (append xs (list (expt (first ys) 2)))
        (rest ys))))
  (iter nil lst))

(defn square-list-2 [lst]
  (map (fn [x] (expt x 2)) lst))

; 2-22
; the last item in 'items' is the last to be cons-ed to the front of 'answer' list.
; interchanging arguments makes the first argument to be of list type.

; 2-23

(defn foreach [f xs]
  (if (empty? xs)
    true
    (do
      (f (car xs))
      (foreach f (cdr xs)))))

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

; 2-27

(defn deep-reverse [xs]
  (if (empty? xs)
    nil
    (append
      (deep-reverse (cdr xs))
      (let [x (car xs)]
        (list (if (list? x)
                (deep-reverse x)
                x))))))

; 2-28

(defn fringe [xs]
  (if (empty? xs)
    nil
    (append
      (let [x (car xs)]
        (if (list? x)
          (fringe x)
          (list x)))
      (fringe (cdr xs)))))

; 2-29

(defn make-mobile [lft rgt]
  (vector lft rgt))

(defn make-branch [len struc]
  (vector len struc))

(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (second mobile))

(defn branch-length [branch]
  (first branch))

(defn branch-structure [branch]
  (second branch))

(declare total-weight)
(defn branch-weight [branch]
  (let [struc (branch-structure branch)]
    (if (number? struc)
      struc
      (total-weight struc))))

(defn total-weight [mobile]
  (+
    (branch-weight (left-branch mobile))
    (branch-weight (right-branch mobile))))

(declare balanced-mobile?)
(defn balanced-branch? [branch]
  (let [struc (branch-structure branch)]
    (if (number? struc)
      true
      (balanced-mobile? struc))))

(defn balanced-mobile? [mobile]
  (defn torque [branch]
    (* (branch-length branch) (branch-weight branch)))
  (let [lft (left-branch mobile)
        rgt (right-branch mobile)]
    (and
      (= (torque lft) (torque rgt))
      (balanced-branch? lft)
      (balanced-branch? rgt))))

; 2-30

(defn square-tree-without-map [tree]
  (if (empty? tree)
    nil
    (append
      (let [x (car tree)]
        (list (if (list? x)
                (square-tree-without-map x)
                (* x x))))
      (square-tree-without-map (cdr tree)))))

(defn square-tree [tree]
  (map
    (fn [x]
      (if (list? x)
        (square-tree x)
        (* x x)))
    tree))

; 2-31

(defn tree-map [f tree]
  (map
    (fn [x]
      (if (list? x)
        (tree-map f x)
        (f x)))
    tree))
