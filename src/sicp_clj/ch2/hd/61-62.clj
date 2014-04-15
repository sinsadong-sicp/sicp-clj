(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 참고 - ordered list에서 intersection set
(defn intersection-set [set1 set2]
    (if (or (empty? set1) (empty? set2))
        '()
        (let [x1 (first set1) x2 (first set2)]
            (cond
                (== x1 x2) (cons x1 (intersection-set (rest set1) (rest set2)))
                (< x1 x2) (intersection-set (rest set1) set2)
                (< x2 x1) (intersection-set set1 (rest set2))))))

; 2-61
(defn adjoin-set [x s]
    (cond
        (empty? s) (list x)
        (== x (first s)) s
        (< x (first s)) (cons x s)
        :else (cons (first s) (adjoin-set x (rest s)))
        ))

; 원래 adjoin은 element-of-set?으로 전체를 한번 다 도는데, 새 adjoin은 처음부터 하나씩 보면서 바로 멈추기 때문에 n/2다.
; (define (adjoin-set x set)
;   (if (element-of-set? x set)
;       set
;       (cons x set)))

; 2-62
(defn union-set [s1 s2]
    (cond
        (empty? s1) s2
        (empty? s2) s1
        :else
            (let [x1 (first s1) x2 (first s2)]
                (cond
                    (== x1 x2) (cons x1 (union-set (rest s1) (rest s2)))
                    (< x1 x2) (cons x1 (union-set (rest s1) s2))
                    (> x1 x2) (cons x2 (union-set s1 (rest s2)))
                    ))))
