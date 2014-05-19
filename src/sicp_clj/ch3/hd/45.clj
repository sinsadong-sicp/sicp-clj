(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-45
; serialized-exchange가 호출되면 (account1 withdraw)와 (account2 deposit)을 부른다.
; 그러나 account1과 account2는 serizlied-exchange에 의해 lock이 걸려있기 때문에 더이상 진행이 안된다.
; 즉 exchange는 withdraw를 부르고, withdraw는 exchange가 끝나야 실행되기 때문에 그냥 무한히 기다리게 된다.
