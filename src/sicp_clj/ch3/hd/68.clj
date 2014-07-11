(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-68

; 안 된다. (pairs integers integers)를 하면 interleave와 pairs를 계속해서 부를 것이다.
; 원래 버전은 pairs가 부르는 cons-stream이 첫 번째 것만 evaluate하도록 만들기 때문에 interleave의 call이 delay되어 괜찮다.