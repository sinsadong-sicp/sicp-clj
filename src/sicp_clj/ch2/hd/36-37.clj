(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only []])
  (:use [clojure.contrib.generic.math-functions :only []]))

(defn accm [op initial sequence]
    (if (empty? sequence) initial
        (op (first sequence) (accm op initial (rest sequence)))))

; 2-36
; 첫 번째 ??는 seqs 안에 있는 각 seq의 first들을 한 list로 모아야 하고
; 두 번째 ??는 ((rest) (rest) (rest))와 같은 list의 list를 만들어야 한다.
; map으로 한방에 해결되는군.
(defn accm-n [op init seqs]
    (if (empty? (first seqs))
        nil
        (cons
            (accm op init (map first seqs))
            (accm-n op init (map rest seqs)))))

(def ss (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
; (prn (accm-n + 0 ss))

; 2-37
(def m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(def n (list (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(def v (list 1 2 3 4))
(def w (list 5 5 5 5))

(defn dot-product [v w]
    (accm + 0 (map * v w)))
; (prn (dot-product v w))

(defn mat-*-vec [m v]
    (map (fn [x] (dot-product x v)) m))
; (prn (mat-*-vec m w))

(defn transpose [m]
    (accm-n cons nil m))
; (prn (transpose m))

(defn mat-*-mat [m n]
    (let [cols (transpose n)]
        (map (fn [x] (mat-*-vec cols x)) m)))
; (prn (mat-*-mat m n))
