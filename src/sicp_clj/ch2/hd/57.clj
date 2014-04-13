(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 2-57
(defn make-sum [a1 a2]
    (cond
        (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
    (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

(defn make-sum-li [li]
    (if (== (count li) 2) (list '+ (first li) (second li))
        (make-sum (first li) (make-sum-li (rest li))))

(defn make-product-li [li]
    (if (== (count li) 2) (list '* (first li) (second li))
        (make-product (first li) (make-product-li (rest li))))
