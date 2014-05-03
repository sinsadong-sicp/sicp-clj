(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-12
; 첫 response는 ('b), 둘째 response는 ('b 'c 'd).
; 그림은 딱히 필요없는듯.
; last-pair의 cdr은 '()임을 생각하면 된다.

; 3-13
; infinite loop가 된다. last-pair를 부르면 if null?을 만족하지 못한다.

; 3-14
; list를 reversing한다.
; v는 여전히 list의 시작 부분을 가리키고 있으므로, v를 출력하면 ('a)가 나온다.
; w는 reverse된 ('d 'c 'b 'a)가 나온다.
