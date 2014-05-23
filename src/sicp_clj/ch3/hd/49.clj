(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-49
; 힌트 그대로다. exchange와 달리 어떤 프로세스가 어떤 리소스를 어떤 순서로 잡을지 결정되어있지 않다면 항상 데드락 문제가 생길 수 있다.
