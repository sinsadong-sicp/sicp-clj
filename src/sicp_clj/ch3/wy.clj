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

; 3-3

(defn make-account [initial-balance password]
  (let [balance (atom initial-balance)]
    (defn withdraw [amount]
      (if (>= @balance amount)
        (swap! balance - amount)
        "Insufficient funds"))
    (defn deposit [amount]
      (swap! balance + amount))
    (defn dispatch [p m]
      (if (= p password)
        (cond
          (= m 'withdraw) withdraw
          (= m 'deposit) deposit
          :else (throw (Exception. "Unknown request -- MAKE-ACCOUNT")))
        (fn [_] "Incorrect password")))
    dispatch))
