(ns sicp-clj.ch3.hy.hy
  (:use sicp-clj.ch3.pair :reload)
  (:use [clojure.contrib.math :only [abs gcd expt sqrt]])
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

;3-23
(defn make-deque []
  (pair nil nil))
(defn front-ptr [deque]
  (.car deque))
(defn rear-ptr [deque]
  (.cdr deque))
(defn empty-deque? [deque]
  (nil? (front-ptr deque)))
(defn set-front! [deque item]
  (.setcar deque item))
(defn set-rear! [deque item]
  (.setcdr deque item))
(defn front-insert-deque! [deque item]
  (let [new-pair (pair (pair item nil) nil)] ;(pair (pair value prev) next)
    (if (empty-deque? deque)
      (do
        (set-front! deque new-pair)
        (set-rear! deque new-pair))
      (do
        (.setcdr new-pair (front-ptr deque))
        (.setcdr (.car (front-ptr deque)) new-pair)
        (set-front! deque new-pair)))))
(defn rear-insert-deque! [deque item]
  (let [new-pair (pair (pair item nil) nil)] ;(pair (pair value prev) next)
    (if (empty-deque? deque)
      (do
        (set-front! deque new-pair)
        (set-rear! deque new-pair))
      (do
        (.setcdr (rear-ptr deque) new-pair)
        (.setcdr (.car new-pair) (rear-ptr deque))
        (set-rear! deque new-pair)))))
(defn front-delete-deque! [deque]
  (if (empty-queue?)
    (throw (Exception. (str "Empty queue delete!")))
    (do
      (set-front! deque (.cdr (front-ptr deque)))
      (if (empty-deque? deque)
        (.setcdr (.car (front-ptr deque)) nil)))))
(defn rear-delete-deque! [deque]
  (if (empty-queue?)
    (throw (Exception. (str "Empty queue delete!")))
    (do
      (set-rear! deque (.car (.cdr (front-ptr deque))))
      (if (nil? (rear-ptr deque))
        (set-front! deque nil)
        (.setcdr (rear-ptr deque) nil)))))
(defn print-deque [deque]
  (defn iter [d s]
    (if (nil? d)
      (println s)
      (do
        (iter (.cdr d) (str s (front-ptr (front-ptr d)) " ")))))
  (iter deque ""))

;3-24
(defn make-table [same-key?]
  (let [local-table (pair '*table* nil)]
    (defn asso [key records]
      (cond
        (nil? records) false
        (same-key? key (.car (.car records))) (.car records)
        :else (asso key (.cdr records))))
    (defn lookup [key1 key2]
      (let [subtable (asso key1 (.cdr local-table))]
        (if subtable
          (let [record (asso key2 (.cdr subtable))]
            (if record
              (.cdr record)
              false))
          false)))
    (defn insert! [key1 key2 value]
      (let [subtable (asso key1 (.cdr local-table))]
        (if subtable
          (let [record (asso key2 (.cdr subtable))]
            (if record
              (.setcdr record value)
              (.setcdr subtable (pair (pair key2 value) (.cdr subtable)))))
          (.setcdr local-table (pair (pair key1 (pair key2 value)) (.cdr local-table))))))
    (defn dispatch [m]
      (cond
        (= m 'lookup-proc) lookup
        (= m 'insert-proc!) insert!
        :else (throw (Exception. "HOYOON"))))
  dispatch))
(defn operation-table [] (make-table))
(defn get-table [] (operation-table 'lookup-proc))
(defn put-table [] (operation-table 'insert-proc!))

;3-27
;global <- env1 { function memo-fib, n = 3 }
;env1 <- env2 { function memo-fib, n = 2 }
;env1 <- env3 { function memo-fib, n = 1 }
;여기서 위에 정의한 table을 통해 예전에 계산된 결과를 이용할 수 있기 때문에 O(n)에 가능.
;다만 (memoize fib) 로 정의할 경우에는, table에서 lookup하는 것과 별개로 fib가 중복으로 n-1, n-2를
;계속 호출하기 때문에 일반 fib와 차이 없다

;3-28
(declare logic-or)
(declare after-delay)
(declare or-gate-delay)
(declare add-action!)
(declare get-signal)
(declare set-signal!)
(defn or-gate [a1 a2 output]
  (defn action[]
    (let [newval (logic-or (get-signal a1) (get-signal a2))]
      (after-delay or-gate-delay
        (fn [] (set-signal! output newval)))))
  (add-action! a1 action)
  (add-action! a2 action))

;3-29
(declare inverter)
(declare and-gate)
(declare make-wire)
(defn or-gate2 [a1 a2 output]
  (let [c1 (make-wire)
        c2 (make-wire)
        c3 (make-wire)]
    (inverter a1 c1)
    (inverter a2 c2)
    (and-gate c1 c2 c3)
    (inverter c3 output)))

;3-30
(declare full-adder)
(defn ripple-carry-adder [a b s c]
  (let [cin (make-wire)]
    (if (nil? (rest a))
      (set-signal! cin 0)
      (ripple-carry-adder (rest a) (rest b) (rest s) cin))
    (full-adder (first a) (first b) cin (first s) c)))

;3-31
;이 구현에서는 signal 변화가 생겨야 그때 procedure가 실행된다.
;또한 procedure 실행 과정에서는 after-delay를 통해 실행될 타이밍인지 체크하는 과정이 들어가 있다.
;이 때 agenda(schedule)에 함수를 등록시키는 식인데, 맨 처음 wire가 0일때는 set-signal!에 의한 c
;all each가 실행되지 않고 따라서 schedule에 함수 수행 정보가 등록되지 않는다.
;propagate했을때 아무것도 못가져옴 ㅇㅇ

;3-32
;??

;3-33
(declare adder)
(declare make-connector)
(declare multiplier)
(declare constant)
(defn averager [a b c]
  (let [sum (make-connector)
        factor (make-connector)]
    (adder a b sum)
    (multiplier c factor sum)
    (constant factor 2)))

;3-34
;(multiplier a a b) 라고 하면 b만 setting하였을 때 코드 안에서 has-value? 체크하는 부분에서
;m1, m2가 둘 다 setting되어 있지 않다.

;3-35
(declare has-value?)
(declare set-value!)
(declare get-value)
(declare forget-value!)
(declare connect)
(defn square [a]
  (* a a))
(defn squarer [a b]
  (declare me)
  (defn process-new-value []
    (if (has-value? b)
      (if (< (get-value b) 0)
        (throw (Exception. "square less than 0 - SQUARER"))
        (set-value! a (sqrt (get-value b)) me))
      (if (has-value? a)
        (set-value! b (square (get-value a)) me))))
  (defn process-forget-value []
    (forget-value! a me)
    (forget-value! b me))
  (defn me [request]
    (cond
      (= request 'value) (process-new-value)
      (= request 'lost) (process-forget-value)
      :else (throw (Exception. "Unknown request - SQUARER"))))
  (connect a me)
  (connect b me)
  me)

;3-37
(declare make-connector)
(defn c- [x y]
  (let [z (make-connector)]
    (adder y x z)
    z))
(defn c* [x y]
  (let [z (make-connector)]
    (multiplier x y z)
    z))
(defn c_div [x y]
  (let [z (make-connector)]
    (multiplier y x z)
    z))
(defn cv [value]
  (let [z (make-connector)]
    (constant z value)
    z))

;3-38
;Peter : P, Paul : A, Mary : M 으로 표기
;(a)
;PAM : 45
;PMA : 35
;APM : 45
;AMP : 50
;MPA : 40
;MAP : 40
;35, 40, 45, 50
;(b) 맨 마지막에 실행한 사람이 P : 110, A : 80, M : 50 이 들어가 있을 것이

;3-39
;총 가짓수는 책에서 봤듯이 101, 121, 110, 11, 100
;이 중 serializer에 의해 P2가 access할때 P1이 동시에 set하는 11, 100은 있을 수 없음 탈락.
;남은 가짓수는 101, 121, 110

;3-40
;100(1번 결과 덮어씀), 1000(2번 결과 덮어씀), 1000000(둘다 실행), 10000(p2먼저 실행된 다음 10*1000했을때), 100000(p1먼저 실행된 다음 p2에서 100*100했을때)
;serialize하면 1000000만 남

;3-41
;아님. withdraw나 deposit순서가 바뀜에 의해 결과가 달라질 수는 있지만 balance reading과는 상관없는 작업들임.

;3-42
;safe 함. 사실 두 솔루션은 concurrency측면에서 차이가 없고, 단지 차이는
;새로 정의된 프로시저가는 call 전에 serialize를 하고 원본은 withdraw나 deposit 호출시에 된다는 점.

;3-44

;Louis의 주장은 구라임.
;Exchange에서는 account들이 모두 idle 상태여야만 한다는 조건이 필요한데, transfer에는 그런 조건이 필요없음.

;3-45

;주어진 코드로 exchange를 하게 되면 코드에서 같은 serializer를 두번 사용하게 된다(serialized-exchange, dispatch).
;그러면 mutex때문에 서로 deadlock

;3-48

;a1, a2에서 a1이 작은 번호라고 하자. 그러면 Paul과 Peter모두 a1을 먼저 require하게 되고, 두 명이 동시에
;a1을 가져올 수는 없으므로 Paul이 Peter를 기다리거나 Peter가 Paul을 기다리는 식으로 deadlock을 피해 순차적으로 해결됨.

;3-49
;48번 문제에서 확인하였듯이, 각자 procedure가 자신이 필요한 resource를 모두 정확히 알고 있다면 그들 사이의
;우선순위를 정해줌으로써 deadlock을 피할 수 있다.

;3-50
(declare stream-null?)
(declare the-empty-stream)
(declare cons-stream)
(declare stream-map)
(declare stream-car)
(declare stream-cdr)
(declare stream-enumerate-interval)
(declare stream-ref)
(comment (
(defn stream-delay [exp]
  (defn memo-proc [proc]
    (let [already-run? (atom false)
          result (atom nil)]
        (fn []
          (if (not already-run?)
            (do
              (reset! already-run? true)
              (reset! result (proc)))))))
  (memo-proc (fn [] exp)))
(defn stream-null? [x]
  (empty? x))
(defn the-empty-stream []
  nil)
(defn cons-stream [a b]
  (cons a (lazy-seq b)))
(defn stream-car [stream]
  (first stream))
(defn stream-cdr [stream]
  (rest stream))
(defn stream-map [proc s]
  (if (stream-null? s)
    (the-empty-stream)
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))
(defn stream-map2 [proc & argstreams]
  (if (stream-null? (first argstreams))
    (the-empty-stream)
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map proc (stream-cdr argstreams)))))
(defn stream-enumerate-interval [low high]
  (if (> low high)
    (the-empty-stream)
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))
(defn stream-ref [s n]
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
))
;3-51
(defn show [x]
  (println x)
  x)
;(def x (stream-map show (stream-enumerate-interval 0 10)))
;(defn x [] (stream-map show (stream-enumerate-interval 0 10)))
;0
;(stream-ref x 5)
;1
;2
;3
;4
;5
;(stream-ref x 7)
;6
;7

;3-52
;(def seq (stream-map accum (stream-enumerate-interval 1 20)))
;1
;(def y (stream-filter even? seq))
;6
;(def z (stream-filter (fn [x] (= (rem x 5) 0)) seq))
;(stream-ref y 7)
;136

;3-53
;책에 나오는 double과 비슷하게, 1부터 시작하는 2의 거듭제곱들(1, 2, 4, 8, ..)

;3-54
(declare add-streams)
(declare ones)
(declare integers)

(comment (
(defn add-streams [s1 s2]
  (stream-map + s1 s2))
(def ones
  (cons-stream 1 ones))
(def integers
  (cons-stream 1 (add-streams ones integers)))
))

(defn mul-streams [s1 s2]
  (stream-map * s1 s2))
;(def factorials (cons-stream 1 (mul-streams factorials integers)))

;3-55
(defn partial-sums [s]
  (cons-stream
    (stream-car s)
    (add-streams (stream-cdr s) (partial-sums s))))

;3-56
(defn merge-streams [s1 s2]
  (cond
    (stream-null? s1) s2
    (stream-null? s2) s1
    :else
      (let [s1car (stream-car s1)
            s2car (stream-car s2)]
        (cond
          (< s1car s2car) (cons-stream s1car (merge-streams (stream-cdr s1) s2))
          (> s1car s2car) (cons-stream s2car (merge-streams s1 (stream-cdr s2)))
          :else (cons-stream s1car (merge-streams (stream-cdr s1) (stream-cdr s2)))))))

;3-57
;memoization을 했기 때문에 추가적인 computation 없이 O(n)에 가능
;안그럴경우 fib(n)을 계산하기 위해서는 fib(n-1) 계산수 + fib(n-2) 계산수 + 1 = fib(n) - 1번의 계산이 필요하다(거의 exponential)

;3-58
;num을 den으로 나눌 때 값을 radix 진법으로 표현한 것
;(expand 1 7 10) => (1 4 2 8 5 7 ...) (1/7 = 0.142857...)
;(expand 3 8 10) => (3 7 5 0 0 0 ...) (3/8 = 0.375)

;3-59
(defn integrate-series [s]
  (stream-map * (stream-map / ones integers) s))
;(def exp-series
;  (cons-stream 1 (integrate-series exp-seires)))
;(def cosine-series
;  (cons-stream 1 (integer-series (scale-stream sine-series -1))))
;(def sine-series
;  (cons-stream 1 (integer-series cosine-series)))

;3-60
(defn mul-series [s1 s2]
  (cons-stream (* (stream-car s1) (stream-car s2))
    (add-streams (mul-streams (stream-cdr s1) (stream-cdr s2)) (mul-series s1 s2))))
