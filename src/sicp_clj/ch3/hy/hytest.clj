(ns sicp-clj.ch3.hy.hytest
  (:refer-clojure)
  (:use sicp-clj.ch3.hy.hy :reload)
  (:use [sicp-clj.ch1.hy.hytest :only [hytest hytest-with-error]]))

;3-1
(println "Chapter 3")
(def A (make-accumulator 10))
(hytest "3-1" 15 (A 5))
(hytest "3-1" 20 (A 5))
