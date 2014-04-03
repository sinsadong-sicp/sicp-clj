(ns sicp-clj.ch2.hy.hytest
  (:refer-clojure :exclude [cons])
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
