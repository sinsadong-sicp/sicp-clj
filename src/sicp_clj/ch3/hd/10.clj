(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-10
; let은 사실 fn과 같으므로, 기존의 (fn [amount])는 (let)으로 만들어진 fn의 안에 만들어진다. 따라서 balance와 amount가 갇힌 환경에서 만들어지게 되고, 결과는 기존의 make-withdraw와 같아진다. 환경 얼개는 크게 다를바가 없을 것 같다.
