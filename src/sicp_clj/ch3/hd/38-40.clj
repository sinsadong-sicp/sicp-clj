(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only [sqr]]))

; 3-38-a)
; 1. 100->110->90->45
; 2. 100->110->55->35
; 3. 100->80->90->45
; 4. 100->80->40->50
; 5. 100->50->60->40
; 6. 100->50->30->40

; 3-38-b)
; 그림은 생략한다. 전부 뒤섞여 돌아간다면, 무언가가 완전히 '무시될 수 있다'고 가정하면 된다.
; 하나가 무시될 때
; 100->50->30
; 100->50->60
; 100->110->55
; 100->110->90
; 100->80->40
; 100->80->90
; 두 개가 무시될 때
; 100->110
; 100->80
; 100->50

; 이렇게 추가된다.

; 3-39
; 이와 같이 시리얼라이즈를 하면, p2가 실행되는 동안에 p1이 돌 수 있기 때문에 10->11->100이 될 수 있다.
; 그리고 원래부터 존재했던 101과 121도 될 수 있다.

; 3-40
; 100, 1000, 10000, 100000, 1000000이 될 수 있다. 중간에 x를 가져오는 동안 끼어들 수 있기 때문에.
; 전부 제대로 synch를 걸면 10 * 100 * 1000 = 1000000 하나뿐이다.