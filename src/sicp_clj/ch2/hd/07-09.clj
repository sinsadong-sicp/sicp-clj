(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only []])
  (:use [clojure.contrib.generic.math-functions :only []]))

; 2-7
(defn make-interval [a b] (cons a b))
(defn lower-bound [i] (first i))
(defn upper-bound [i] (last i))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; 2-8
(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; 2-9
(defn width-interval [i]
  (/ (- (upper-bound i) (lower-bound)) 2))

; 두 interval [a, b]와 [c, d]를 add한 것의 width는 (b - a + d - c) / 2
; [a, b]의 width는 (b - a) / 2, [c, d]의 width는 (d - c) / 2
; width(int1, int2) = width(int1) + width(int2)
; 두 interval [a, b]와 [c, d]를 sub한 것의 width는 (b - a + d - c) / 2, add와 같음.

; multiply는 {max(ac, ad, bc, bd) - min(ac, ad, bc, bd)}인데 width로는 이를 표한할 수 없음.
; width(int1) * width(int2)를 해봐도 max와 min을 이용해 뭔가 할 수는 없다.
