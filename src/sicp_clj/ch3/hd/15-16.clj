(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-15
; z1: x의 car가 가리키는 것이 a에서 wow로 변함. 나머지는 그림 3.16과 동일.
; z2: z2의 car이 가리키는 리스트의 car이 wow를 기리킴. 나머지는 그림 3.17과 동일.

; 3-16
; 기본적으로 서로 다른 물체가 pair를 share할 수 있다는 것을 무시한 채 짠 거라서, shared pair를 쓰는 물체를 제대로 연산할 수 없다.
; (def x '(a))
; (def y (cons x x))
; (def z (cons y y))
; (count-pairs '(a b c)) -> 3
; (count-pairs y) -> 4
; (count-pairs z) -> 7
; cycle을 만들어버리면 infinite하계 계산한다.
