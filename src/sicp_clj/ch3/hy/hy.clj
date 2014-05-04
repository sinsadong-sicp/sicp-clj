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
      (defn increase-bad [x]
        (do
          (swap! attempt inc)
          (if (>= @attempt 2)
            (call-the-cop)
            "Incorrect password")))
      (if (= pw @passwd)
        (cond
          (= m 'withdraw) withdraw
          (= m 'deposit) deposit
          (= m 'bad-passwd) increase-bad
          :else (throw (Exception. "Unknown request - MAKE-ACCOUNT" m)))
        increase-bad
        ))
  dispatch))

;3-5
(defn monte-carlo [trials experiment]
  (defn iter [trials-remaining trials-passed]
    (cond
      (= trials-remaining 0) (/ trials-passed trials)
      (experiment) (iter (dec trials-remaining) (inc trials-passed))
      :else (iter (dec trials-remaining) trials-passed)))
  (iter trials 0))
(defn random-in-range [low high]
  (let [range (- high low)]
    (+ low (rand range))))
(defn estimate-integral [p x1 x2 y1 y2 trials]
  (*
    (* (- x2 x1) (- y2 y1))
    (monte-carlo
      trials
      (fn []
        (p (random-in-range x1 x2)
           (random-in-range y1 y2))
      )
    )))

;3-7
(defn make-joint [account oldpasswd newpasswd]
  (fn [pass m]
    (if (= pass newpasswd)
      (account oldpasswd m)
      (account newpasswd 'bad-passwd))))

;3-8
(defn f1 [value1]
  (defn f2 [value2]
    (let [value3 (atom value1)]
      (swap! value3 = value2)
      @value3))
  f2)
(defn f [x] ((f1 0) x))
