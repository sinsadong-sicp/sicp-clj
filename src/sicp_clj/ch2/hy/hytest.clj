(ns sicp-clj.ch2.hy.hytest
  (:refer-clojure)
  (:use sicp-clj.ch2.hy.hy :reload)
  (:use [sicp-clj.ch1.hy.hytest :only [hytest hytest-with-error]]))
(println "Chapter 2")
;2-1
(hytest "2-1" 2 (car (make-rat 4 6)))
;2-2
(hytest "2-2" 0.5 (double (car (midpoint-segment (make-segment (make-point 0 0 ) (make-point 1 1))))))
;2-3
(hytest "2-3" 1 (area-rect (make-rect (make-point 0 0) (make-point 1 1))))
;2-4
(hytest "2-4" 1 (car2 (cons2 1 2)))
(hytest "2-4" "JUN" (car2 (cons2 "JUN" "HOYOON")))
;2-5
(hytest "2-5" 2 (car3 (cons3 2 3)))
(hytest "2-5" 3 (cdr3 (cons3 2 3)))
;2-6
(hytest "2-6" 4 (((one) square) 2))
(hytest "2-6" 16 (((two) square) 2))
(hytest "2-6" 256 (((add (one) (two)) square) 2))
;2-7
(hytest "2-7" 2 (upper-bound (make-interval 0 2)))
(hytest "2-7" 6 (upper-bound (add-interval (make-interval 0 4) (make-interval 0 2))))
(hytest "2-7" 8 (upper-bound (mul-interval (make-interval 0 4) (make-interval 0 2))))
(hytest "2-7" 4.0 (upper-bound (div-interval (make-interval 1 4) (make-interval 1 2))))
;2-8
(hytest "2-8" 4 (upper-bound (sub-interval (make-interval 0 4) (make-interval 0 2))))
;2-9
(hytest "2-9" 2 (width-interval (make-interval 0 4)))
(hytest "2-9" 0 (width-interval (make-interval 1 1)))
;2-10
(try
  (hytest "2-10" 0 (upper-bound (div-interval2 (make-interval 0 4) (make-interval 1 1))))
  (catch Exception e
    (println "PASS" )))
;2-11
(hytest "2-11" 8 (upper-bound (mul-interval2 (make-interval 0 4) (make-interval 0 2))))
;2-12
(hytest "2-12" 9 (lower-bound (make-center-percent 10 10)))
;2-17
(hytest "2-17" (list 34) (last-pair (list 23 72 34)))
;2-18
(hytest "2-18" (list 3 2 1) (reverse-list (list 1 2 3)))
;2-20
(hytest "2-20" (list 1 3) (same-parity 1 3 6 8))
;2-21
(hytest "2-21" (list 4 9) (square-list1 (list 2 3)))
;2-23
;(for-each (fn [x] (println x)) (list 1 3))고
;2-25
(hytest "2-25" 7 (first (rest (first (rest (rest (list 1 3 (list 5 7) 9)))))))
(hytest "2-25" 7 (first (first (list (list 7)))))
(hytest "2-25" 7 (first (rest (first (rest (first (rest (first (rest (first (rest (first (rest (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))))
;2-26
(hytest "2-26" (list 1 2 3 4 5 6) (apply list (concat x y)))
(hytest "2-26" (list (list 1 2 3) 4 5 6) (cons x y))
(hytest "2-26" (list (list 1 2 3) (list 4 5 6)) (list x y))
;2-27
(hytest "2-27" (list (list (list 4 3) (list 2 1))) (list (deep-reverse (list (list 1 2) (list 3 4)))))
;2-28
(hytest "2-28" (list 1 2 3 4) (fringe (list (list 1 2) (list 3 4))))
