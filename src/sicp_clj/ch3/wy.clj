(ns sicp-clj.ch3.wy
  (:use [clojure.contrib.math :only [gcd]]))

; 3-1

(defn make-accumulator [initial]
  (let [acc (atom initial)]
    (fn [x]
      (swap! acc + x))))

; 3-2

(defn make-monitored [f]
  (let [counter (atom 0)]
    (fn [x]
      (cond
        (= x 'how-many-calls?) @counter
        (= x 'reset-counter) (reset! counter 0)
        :else (do
                (swap! counter inc)
                (f x))))))

; 3-3, 4

(declare call-the-cops)
(defn make-account [initial-balance password]
  (let [balance (atom initial-balance)
        suspicious-attempts (atom 0)]
    (defn withdraw [amount]
      (if (>= @balance amount)
        (swap! balance - amount)
        "Insufficient funds"))
    (defn deposit [amount]
      (swap! balance + amount))
    (defn dispatch [p m]
      (if (= p password)
        (do
          (reset! suspicious-attempts 0)
          (cond
            (= m 'withdraw) withdraw
            (= m 'deposit) deposit
            :else (throw (Exception. "Unknown request -- MAKE-ACCOUNT"))))
        (fn [_]
          (do
            (swap! suspicious-attempts inc)
            (if (> @suspicious-attempts 7)
              (call-the-cops)
              "Incorrect password")))))
    dispatch))

; 3-5

(defn monte-carlo [trials experiment]
  (defn iter [trials-remaining trials-passed]
    (cond
      (zero? trials-remaining) (/ trials-passed trials)
      (experiment) (iter (dec trials-remaining) (inc trials-passed))
      :else (iter (dec trials-remaining) trials-passed)))
  (iter trials 0))

(defn random-in-range [low high]
  (let [n (- high low)]
    (+ low (rand n))))

(defn estimate-integral [p x1 x2 y1 y2 trials]
  (let [area (* (- x2 x1) (- y2 y1))]
    (defn experiment []
      (let [x (random-in-range x1 x2)
            y (random-in-range y1 y2)]
        (p x y)))
    (* area (monte-carlo trials experiment))))

; 3-7

(defn make-joint [account original-password new-password]
  (fn [p m]
    (if (= p new-password)
      (account original-password m)
      (fn [_] "Incorrect password"))))

; 3-8

(def f
  (let [a (atom 0)]
    (defn g [x]
      (let [b (atom @a)]
        (swap! a + x)
        @b))
    g))

; left-to-right evaluation
; (+ (f 0) (f 1))
; => (+ 0 (f 1))
; => (+ 0 0)
; => 0

; right-to-left evaluation
; (+ (f 0) (f 1))
; => (+ (f 0) 1)
; => (+ 0 1)
; => 1

; 3-9

; recursive
; global <- E1 (n: 6) [body of factorial]
; global <- E2 (n: 5) [body of factorial]
; global <- E3 (n: 4) [body of factorial]
; global <- E4 (n: 3) [body of factorial]
; global <- E5 (n: 2) [body of factorial]
; global <- E6 (n: 1) [body of factorial]

; iterative
; global <- E1 (n: 6) [body of factorial]
; global <- E2 (product: 1, counter: 1, max-count: 6) [body of iter]
; global <- E3 (product: 1, counter: 2, max-count: 6) [body of iter]
; global <- E4 (product: 2, counter: 3, max-count: 6) [body of iter]
; global <- E5 (product: 6, counter: 4, max-count: 6) [body of iter]
; global <- E6 (product: 24, counter: 5, max-count: 6) [body of iter]
; global <- E7 (product: 120, counter: 6, max-count: 6) [body of iter]
; global <- E8 (product: 720, counter: 7, max-count: 6) [body of iter]

; 3-10

; (def W1 (make-withdraw 100))
; global <- E1 (initial-amount: 100) [body of make-withdraw]
; E1 <- E2 (balance: 100) [body of lambda]

; (W1 50)
; global <- E1 (initial-amount: 100) [body of make-withdraw]
; E1 <- E2 (balance: 50) [body of lambda]

; (def W2 (make-withdraw 100))
; global <- E1 (initial-amount: 100) [body of make-withdraw]
; E1 <- E2 (balance: 50) [body of lambda]
; global <- E3 (initial-amount: 100) [body of make-withdraw]
; E3 <- E4 (balance: 100) [body of lambda]

; 3-11

; (def acc (make-account 50))
; global <- E1 (balance: 50, withdraw: ..., deposit: ..., dispatch: ...) [body of make-account]

; ((acc 'deposit) 40)
; global <- E1 (balance: 50, withdraw: ..., deposit: ..., dispatch: ...) [body of make-account]
; E1 <- E2 (m: 'deposit) [body of dispatch]
; E1 <- E3 (amount: 40) [body of deposit]

; ((acc 'withdraw) 60)
; global <- E1 (balance: 90, withdraw: ..., deposit: ..., dispatch: ...) [body of make-account]
; E1 <- E4 (m: 'withdraw) [body of dispatch]
; E1 <- E5 (amount: 60) [body of withdraw]

; local state for acc is kept in E1

; (def acc2 (make-account 100))
; global <- E6 (balance: 100, withdraw: ..., deposit: ..., dispatch: ...) [body of make-account]

; local state for acc2 is kept separately in E6
