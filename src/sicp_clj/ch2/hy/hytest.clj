(ns sicp-clj.ch2.hy.hytest
	(:use sicp-clj.ch2.hy.hy)
	(:use [sicp-clj.ch1.hy.hytest :only [hytest hytest-with-error]]))
(println "Chapter 2")
;2-1
(hytest "2-1" 2 (first (make-rat 4 6)))
