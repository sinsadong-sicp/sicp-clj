(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only [expt gcd]])
  (:use [clojure.contrib.generic.math-functions :only []]))

; 2-4
; (defn cons-hd [x y]
;   (fn [m] (m x y)))

; (defn car [z]
;   (z (fn [p q] p)))

; (car (cons-hd x y))
; ((cons-hd x y) (fn [p q] p)) -> car 대입
; ((fn [m] (m x y) (fn [p q] p))) -> cons-hd 대입
; ((fn [p q] p) x y) -> fn p q p가 m으로 들어감
; (x) -> fn p q 계산

; (defn cdr [z]
;   (z (fn [p q] q)))

; 2-5
(defn cons-int [a b]
  (* (expt 2 a) (expt 3 b)))

(defn car-int [c]
  (if
    (odd? c) 0
    (inc (car-int (/ c 2)))))

(defn cdr-int [c]
  (if
    (== (gcd c 3) 1) 0
    (inc (cdr-int (/ c 3)))))

(prn (cdr-int (cons-int 2 5))) ; 5
(prn (car-int (cons-int 2 5))) ; 2

; 2-6
(def zero-hd (fn [f] (fn [x] x)))
(defn inc-hd [n] (fn [f] (fn [x] (f ((n f) x)))))

; (inc-hd zero-hd)
; ((fn [f] (fn [x] (f ((n f) x)))) zero-hd)
; (fn [f] (fn [x] (f ((zero-hd f) x)))) -> ((zero-hd something) x)는 그냥 x다.
; (fn [f] (fn [x] (f x)))

(def one-hd
  (fn [f] (fn [x] (f x))))

; (inc-hd one-hd)
; (fn [f] (fn [x] (f ((one-hd f) x)))) -> ((one-hd something) x)는 (something x)
; (fn [f] (fn [x] (f (f x))))

(def two-hd
  (fn [f] (fn [x] (f (f x)))))

(defn plus-hd [a b]
  (fn [f] (fn [x]
    ((a f) ((b f) x))))) ; f가 b개 붙은 (f (f (f x)))걸 받아서 거기에 f를 a개 더 붙인다
