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
