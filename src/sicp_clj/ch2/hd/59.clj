(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 2-59
; element-of-set? 은 contains?로 대체 가능
; adjoin-set은 conj로 대체 가능 (conj #{1 2 3} 1) = #{1 2 3}
; intersection과 union도 존재하긴 하지만..
(defn intersection-set [set1 set2]
    (cond
        (or (empty? set1) (empty? set2)) '()
        (contains? set2 (first set1))
            (cons (first set1) (intersection-set (rest set1) set2))
        :else (intersection-set (rest set1) set2)))

(defn union-set [s1 s2]
    (cond
        (empty? s1) s2
        (not (contains? s2 (first s1))) (cons (first s1) (union-set (rest s1) s2))
        :else (union-set (rest s1) s2)))
