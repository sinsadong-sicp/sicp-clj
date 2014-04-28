(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-3
; atom은 숫자와 비교할 수 없다.
; atom 값을 부를 때 @를 붙이지 않으면 이런 식으로 나온다.
;   #<Atom@5d09823a: 10>
(defn make-account [balance password]
    (let [bal (atom balance)]
        (defn withdraw [amount]
            (if (>= balance amount)
                (do (swap! bal - amount) @bal)
                "Insufficient funds"))
        (defn deposit [pw amount]
            (do (swap! bal + amount) @bal))
        (defn dispatch [pw msg]
            (cond
                (not= pw password) (fn [x] "Incorrect password")
                (= msg 'withdraw) withdraw
                (= msg 'deposit) deposit
                :else (prn "Unknown request"))))
    dispatch)
