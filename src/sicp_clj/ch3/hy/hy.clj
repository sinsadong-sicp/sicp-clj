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
  (let [value4 (atom value1)]
    (defn f2 [value2]
      (let [value3 (atom @value4)]
        (swap! value4 + value2)
        @value3))
    f2))

;3-9
(comment (
  global <- env1 { function factorial, n : 6 }
  global <- env2 { function factorial, n : 5 }
  global <- env3 { function factorial, n : 4 }
  global <- env4 { function factorial, n : 3 }
  global <- env5 { function factorial, n : 2 }
  global <- env6 { function factorial, n : 1 }
))
(comment (
  global <- env1 { function factorial, n : 6 }
  global <- env2 { function fact-iter, product : 1, counter : 1, max-count : 6 }
  global <- env3 { function fact-iter, product : 1, counter : 2, max-count : 6 }
  global <- env4 { function fact-iter, product : 2, counter : 3, max-count : 6 }
  global <- env5 { function fact-iter, product : 6, counter : 4, max-count : 6 }
  global <- env6 { function fact-iter, product : 24, counter : 5, max-count : 6 }
  global <- env7 { function fact-iter, product : 120, counter : 6, max-count : 6 }
  global <- env7 { function fact-iter, product : 720, counter : 7, max-count : 6 }
))

;3-10
(comment (
  global <- env1 { function make-withdraw, initial-amount : 100 }
  env1 <- { function lambda, balance : 100 }
  env1 <- env2 { function lambda, amount : 50, balance : 50 }
  global <- env3 { function make-withdraw, initial-amount : 100 }
))

;3-11
(comment (
  ;(define acc (make-account 50))
  global <- env1 { function make-account, balance : 50 }
))
(comment (
  ;((acc 'deposit) 40)
  global <- env1 { function make-account, balance : 50 }
  env1 <- env2 { function dispatch, m : 'deposit }
  env2 <- env3 { function deposit, amount : 40 }
))
(comment (
  ;((acc 'withdraw) 60)
  global <- env1 { function make-account, balance : 90 }
  env1 <- env2 { function dispatch, m : 'withdraw }
  env2 <- env3 { function withdraw, amount : 60 }
))
(comment (
  Local state is kept in env1
  For acc2, another env (env4?) is made, which has different local state 'balance'
  Function definitions are the same
))

;3-12
(comment (
  (define x (list ’a ’b))
  (define y (list ’c ’d))
  (define z (append x y))
  z
    (a b c d)
  (cdr x)
    (b)
  (define w (append! x y)) w
    (a b c d)
  (cdr x)
    (b c d)
  why? set-cdr! modified x
))

;3-13
(comment(
  a <- b <- c <- a 의 구조임
  (last-pair z) 는 무한루프
))
