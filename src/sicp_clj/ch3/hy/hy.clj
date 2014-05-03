(ns sicp-clj.ch3.hy.hy
  (:refer-clojure))

;3-1
(defn make-accumulator [init]
  (let [sum (atom init)]
    (defn addition [addend]
      (swap! sum + addend)))
    addition)

;3-2
(defn make-monitored [func]
  (let [cnt (atom 0)]
    (defn count-func [x]
      (cond
        (= 'how-many-calls? x) @cnt
        (= 'reset-count x) (reset! cnt 0)
        :else (do
          (swap! cnt inc)
          (func x)))))
  count-func)

;3-3, 3-4
(defn call-the-cop []
  "Police officer")
(defn make-account [init-balance init-passwd]
  (let [balance (atom init-balance)
        passwd (atom init-passwd)
        attempt (atom 0)]
    (defn withdraw [amount]
      (if (>= @balance amount)
        (do
          (swap! balance - amount)
          @balance)
        "Insufficient funds"))
    (defn deposit [amount]
      (swap! balance + amount)
      @balance)
    (defn dispatch-before [pw m] ;3-3
      (if (= pw @passwd)
        (cond
          (= m 'withdraw) withdraw
          (= m 'deposit) deposit
          :else (throw (Exception. "Unknown request - MAKE-ACCOUNT" m)))
        (fn [x] "Incorrect password")
        ))
    (defn dispatch [pw m] ;3-4
      (if (= pw @passwd)
        (cond
          (= m 'withdraw) withdraw
          (= m 'deposit) deposit
          :else (throw (Exception. "Unknown request - MAKE-ACCOUNT" m)))
        (fn [x]
          (do
            (swap! attempt inc)
            (if (>= @attempt 2)
              (call-the-cop)
              "Incorrect password")))
        ))
  dispatch))
