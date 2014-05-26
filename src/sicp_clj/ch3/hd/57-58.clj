(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-57
; 3-27에서도 풀었듯, memoize를 해놓으면 fib를 구하는데 O(n)으로 가능하다. n-1번 계산하면 된다.
; memoize를 안하면 대략 O(2^n), 완전 정확히는 N(x) = N(x - 1) + N(x - 2) + 1 을 substitution method로 풀어서 O(1.618... ^ n)이 된다.

; 3-58
; (den / num)을 radix 진법으로 표현했을 때 그 표현을 소수점 첫째자리부터 표현하는 스트림.
; (expand 1 7 10)은 1 4 2 8 5 7 ...
; (expand 3 8 10)은 3 7 5 0 0 0 ...
