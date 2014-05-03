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
(hytest "3-3" "Police officer" ((C 'some-other-password 'withdraw) 50))
