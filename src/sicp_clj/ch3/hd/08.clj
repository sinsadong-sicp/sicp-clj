(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-8
(def f
   (let [value (atom 2)]
     (fn [x]
        (if (= @value 2) (reset! value x) 0))))
; 만들긴 했는데 돌아가는지 확인할 방법이 없군.
