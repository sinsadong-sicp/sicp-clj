(ns sicp-clj.ch3.hy.hy
  (:use sicp-clj.ch3.pair :reload)
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
;(comment
;  global <- env1 { function factorial, n : 6 }
;  global <- env2 { function factorial, n : 5 }
;  global <- env3 { function factorial, n : 4 }
;  global <- env4 { function factorial, n : 3 }
;  global <- env5 { function factorial, n : 2 }
;  global <- env6 { function factorial, n : 1 }
;)
;(comment (
;  global <- env1 { function factorial, n : 6 }
;  global <- env2 { function fact-iter, product : 1, counter : 1, max-count : 6 }
;  global <- env3 { function fact-iter, product : 1, counter : 2, max-count : 6 }
;  global <- env4 { function fact-iter, product : 2, counter : 3, max-count : 6 }
;  global <- env5 { function fact-iter, product : 6, counter : 4, max-count : 6 }
;  global <- env6 { function fact-iter, product : 24, counter : 5, max-count : 6 }
;  global <- env7 { function fact-iter, product : 120, counter : 6, max-count : 6 }
;  global <- env7 { function fact-iter, product : 720, counter : 7, max-count : 6 }
;))

;3-10
;(comment (
;  global <- env1 { function make-withdraw, initial-amount : 100 }
;  env1 <- { function lambda, balance : 100 }
;  env1 <- env2 { function lambda, amount : 50, balance : 50 }
;  global <- env3 { function make-withdraw, initial-amount : 100 }
;))

;3-11
;(comment (
  ;(define acc (make-account 50))
;  global <- env1 { function make-account, balance : 50 }
;))
;(comment (
  ;((acc 'deposit) 40)
;  global <- env1 { function make-account, balance : 50 }
;  env1 <- env2 { function dispatch, m : 'deposit }
;  env2 <- env3 { function deposit, amount : 40 }
;))
;(comment (
;  ;((acc 'withdraw) 60)
;  global <- env1 { function make-account, balance : 90 }
;  env1 <- env2 { function dispatch, m : 'withdraw }
;  env2 <- env3 { function withdraw, amount : 60 }
;))
;(comment (
;  Local state is kept in env1
;  For acc2, another env (env4?) is made, which has different local state 'balance'
;  Function definitions are the same
;))

;3-12
;(comment (
;  (define x (list ’a ’b))
;  (define y (list ’c ’d))
;  (define z (append x y))
;  z
;    (a b c d)
;  (cdr x)
;    (b)
;  (define w (append! x y)) w
;    (a b c d)
;  (cdr x)
;    (b c d)
;  why? set-cdr! modified x
;))

;3-13
;(comment(
;  a <- b <- c <- a 의 구조임
;  (last-pair z) 는 무한루프
;));

;3-14
;(comment (
;  v <- {a b c d}
;  여기서 mystery v를 수행하면
;  v <- {a} (set-cdr!에 의해 맨 앞만 남음)
;  w <- {d c b a} (inverse가 된다능)
;))

;3-15
;(comment (
;z1 -> (car | cdr)
;        |     |
;x ->  (car | cdr-)---(-car | nil)
;        |               |
;       wow              b
;(같이바뀜)
;z2 -> (car | cdr-)---(-car | cdr-)---(car | nil)
;        |               |              |
;        |               a              b
;        |               |              |
;       wow           (-car | cdr-)---(car | nil)
;(혼자바뀜)
;))

;3-16
;(comment (
;  평범한 (a b c) list -> 3
;  3-15 z1 처럼
;  x <- (cons a b)
;  y <- (cons x x) 하면 ((a b) a b) 로 3 pair짜리 리스트지만 세보면 4
;  비슷하게
;  z <- (cons y y) 하면 (((a b) a b) (a b) (a b)) => 7
;  만약
;  a <- (a b c) 하고 마지막 c가 다시 a를 point하게 만들면 => 무한루프)
;))

;3-17
;http://goo.gl/koKHt
(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn count-pairs [x]
  (let [visited (atom '())]
    (defn iter [y]
      (if (or (not (list? y)) (in? y visited))
        0
        (do
          (reset! visited (list y @visited))
          (+ (iter (first y)) (iter (rest y)) 1)))))
  (iter x))

;3-18
(defn checkcycle [l]
  (let [visited (atom nil)]
  (defn iter [x]
    (reset! visited (list x @visited))
    (cond
      (nil? (rest x)) false
      (in? (rest x)) true
      :else (iter (rest x)))
    )))

;3-19
;http://goo.gl/u3ZDf
(defn checkcycle2 [l]
  (defn totoise [l1]
    (if (list? l1)
      (rest l1)
      '()))
  (defn hare [l2]
    (totoise (totoise l2)))
  (defn iter [t h]
    (cond
      (not (list? t)) false
      (= t h) false
      (= t (totoise h)) false
      :else (iter (totoise t) (hare h))))
  (iter (totoise l) (hare l)))

;3-20
; (define x (cons 1 2))
;global <- env1 x = { function cons, x = 1, y = 2 }
; (define z (cons x x))
;global <- env2 z = { function cons, x = x, y = x }
; (set-car! (cdr z) 17)
;env2 <- env3 { function dispatch, m = 'cdr }
;env3 <- env4 { function set-x!, z = (cdr z), new-value = 17 }
;env1 <- env5 { function set!, x = 17 }
; (car x)
;env5 <- env6 { function car, z = x }

;3-21
(declare car)
(defn print-queue [queue]
  (car queue))

;3-22
(defn make-queue []
  (let [front-ptr (atom nil)
        rear-ptr (atom nil)]
    (defn set-front-ptr! [item]
      (reset! front-ptr item))
    (defn set-rear-ptr! [item]
      (reset! rear-ptr item))
    (defn empty-queue? []
      (nil? @front-ptr))
    (defn front-queue []
      (if (empty-queue?)
        (throw (Exception. (str "Empty queue!!")))
        (.car front-ptr)))
    (defn insert-queue! [item]
      (let [new-pair (pair item nil)]
        (if (empty-queue?)
          (do
            (set-front-ptr! new-pair)
            (set-rear-ptr! new-pair))
          (do
            (.setcdr @rear-ptr new-pair)
            (set-rear-ptr! new-pair)))))
    (defn delete-queue! []
      (if (empty-queue?)
        (throw (Exception. (str "Empty queue delete!")))
        (set-front-ptr! (.cdr @front-ptr))))
    (defn print-queue []
      (println @front-ptr))
    (defn dispatch [m]
      (cond
        (= m 'empty-queue?) empty-queue?
        (= m 'front-queue) front-queue
        (= m 'insert-queue!) insert-queue!
        (= m 'delete-queue!) delete-queue!
        (= m 'print-queue) print-queue
        :else (println "error")))
  dispatch))
