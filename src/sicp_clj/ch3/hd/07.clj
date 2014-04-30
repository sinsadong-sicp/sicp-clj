(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-07
(defn make-account [balance password]
    (let [bal (atom balance)]
        (defn withdraw [amount]
            (if (>= balance amount)
                (do (swap! bal - amount) @bal)
                "Insufficient funds"))
        (defn deposit [amount]
            (do (swap! bal + amount) @bal))
        (defn dispatch [pw msg]
            (cond
                (not= pw password) (fn [x] "Incorrect password")
                (= msg 'withdraw) withdraw
                (= msg 'deposit) deposit
                :else (prn "Unknown request"))))
    dispatch)

; 새 alias로 기존 암호로 접근해도 가능하게 했다.
(defn make-joint [account orig-pw another-pw]
    (if (number? ((account orig-pw 'withdraw) 0))
        (fn [pw msg]
            (if (= pw another-pw)
                (account orig-pw msg)
                (account pw msg)))
        (prn "Incorrect password")
        ))

; test
(def acc (make-account 100 'ab))
(def newacc (make-joint acc 'ab 'bc))
