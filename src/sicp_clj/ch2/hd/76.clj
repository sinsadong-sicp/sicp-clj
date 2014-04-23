(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 2-76
; generic operations with explicit dispatch
; type 추가될 때 기존의 proc 구현에 각 타입에 대한 정보를 넣어야 한다.
; op 추가될 때는 해당 op가 모든 type을 서포트하게 만들면 된다.

; data-directed style
; type 추가될 때 기존 proc는 안 건드리고 table에 새로운 type column을 추가하고, 필요할 때는 그 type을 install해서 쓴다.
; op 추가될 때는 기존 proc에 각각 추가해야 한다.

; message-passing-style
; type 추가되면 그 type을 처리하는 dispatch 함수를 내부적으로 가지는 procedure형 data를 만들어야 한다.
; op 추가되면 각 데이터가 그 op를 어떻게 다룰 것인지를 추가한다.

; type을 자주 추가한다면 data-directed가 좋다.
; operation을 자주 추가한다면 msg passing이 간단해 보이긴 하는데.. 확실히는 모르겠다.
