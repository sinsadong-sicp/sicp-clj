(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-4
; direction과 조금은 다르지만 괜찮겠지.
; 어제 atom과 숫자를 비교할 수 없다고 썼는데, 그냥 이것도 @를 붙이면 된다.
(defn make-account [balance password]
    (let [bal (atom balance) trials (atom 0)]
        (defn withdraw [amount]
            (if (>= balance amount)
                (do (swap! bal - amount) @bal)
                "Insufficient funds"))
        (defn deposit [amount]
            (do (swap! bal + amount) @bal))
        (defn incorrect-password [x]
            (if (>= @trials 7)
                "Cops called"
                (do (swap! trials inc) "Incorrect password")))
        (defn dispatch [pw msg]
            (if (not= pw password)
                incorrect-password
                (do
                    (reset! trials 0)
                    (cond
                        (= msg 'withdraw) withdraw
                        (= msg 'deposit) deposit
                        :else (prn "Unknown request")))))
        )
    dispatch)
