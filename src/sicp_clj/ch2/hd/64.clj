(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 2-64
; (defn make-tree [entry left right]
;   (list entry left right))

; (defn partial-tree [elts n]
; 	(if (zero? n)
; 		(cons '() elts)
; 		(let [left-size (quotient (- n 1) 2)]
; 			(let [left-result (partial-tree elts left-size)]
; 				(let [left-tree (first left-result)
; 					non-left-elts (rest left-result)
; 					right-size (- n (inc left-size))]
; 					(let [this-entry (first non-left-elts)
; 						right-result (partial-tree (rest non-left-elts) right-size)]
; 						(let [right-tree (first right-result)
; 							remaining-elts (rest right-result)]
; 							(cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

; (defn list->tree [elem]
; 	(first (partial-tree elem (count elem))))

; a. partial-tree는 elts를 this-entry (median), median보다 작은 아이템들의 list, median보다 큰 아이템들의 list로 나눠서 binary tree를 만든다. tree의 root는 this-entry고, 왼쪽 subtree는 median보다 작은 아이템들의 list로 이루어진 partial-tree이고 오른쪽은 큰 아이템들의 list로 이루어진 partial-tree이다. 그리기는 귀찮다.

; b. partial-tree는 매번 1/2로 줄어들고 그걸 두 partial tree에 대해서 하니까, theta(n)이다.