(ns sicp-clj.ch3.hy.hytest
  (:refer-clojure)
  (:use sicp-clj.ch3.hy.hy :reload)
  (:use [clojure.contrib.math :only [abs gcd expt sqrt]])
  (:use [sicp-clj.ch1.hy.hytest :only [hytest hytest-with-error]]))

;3-1
(println "Chapter 3")
(def A (make-accumulator 10))
(hytest "3-1" 15 (A 5))
(hytest "3-1" 20 (A 5))

;3-2
(def B (make-monitored sqrt))
(hytest "3-2" 2 (B 4))
(hytest "3-2" 1 (B 'how-many-calls?))

;3-3
(def C (make-account 100 'secret-password))
(hytest "3-3" 60 ((C 'secret-password 'withdraw) 40))
(hytest "3-3" "Incorrect password" ((C 'some-other-password 'withdraw) 50))

;3-4
(hytest "3-4" "Police officer" ((C 'some-other-password 'withdraw) 50))

;3-5
(hytest-with-error "3-5" 3.14 (* 4.0 (estimate-integral
    (fn [x y]
      (if (<= (+ (* x x) (* y y)) 1)
        true
        false
      )) 0 1 0 1 6000))) ;10000하니까 stackoverflow난다 왜지;; 내 코드가 비효율적으로 짜져있는듯

;3-7
(def D (make-account 100 'secret-password))
(def D2 (make-joint D 'secret-password 'new-password))
(def D3 (make-joint D 'some-password 'secret-password))
(hytest "3-7" 60 ((D 'secret-password 'withdraw) 40))
(hytest "3-7" 20 ((D2 'new-password 'withdraw) 40))
(hytest "3-7" "Incorrect password" ((D2 'some-password 'withdraw) 40))
(hytest "3-7" "Police officer" ((D3 'secret-password 'withdraw) 40))

;3-8
(def hy1 (f1 0))
(def hy2 (f1 0))
(hytest "3-8" 0 (+ (hy1 0) (hy1 1)))
(hytest "3-8" 1 (+ (hy2 1) (hy2 0)))

;3-22
(def queue (make-queue))
((queue 'insert-queue!) 1)
((queue 'insert-queue!) 2)
((queue 'insert-queue!) 3)
((queue 'print-queue))
((queue 'delete-queue!))
((queue 'print-queue))

;(use 'sicp-clj.ch3.hy.hytest :reload-all)
