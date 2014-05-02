(ns sicp-clj.ch3.wy)

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
                (swap! counter + 1)
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
