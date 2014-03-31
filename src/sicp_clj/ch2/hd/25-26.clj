(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only []])
  (:use [clojure.contrib.generic.math-functions :only []]))

; 2-25
(def x [1 3 [5 7] 9])
(prn (first (rest (first (rest (rest x))))))

(def y [[7]])
(prn (first (first y)))

(def z [1 [2 [3 [4 [5 [6 7]]]]]])
(prn (first (rest (first (rest (first (rest (first (rest (first (rest (first (rest z)))))))))))))

; 2-26
; append: [1 2 3 4 5 6]
; cons: [[1 2 3] 4 5 6]
; list: [[1 2 3] [4 5 6]]
