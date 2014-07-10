(ns sicp-clj.ch3.wy
  (:use sicp-clj.ch3.pair)
  (:use [clojure.contrib.math :only [gcd sqrt expt]]))

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
            :else (throw (Exception. (str "Unknown request -- MAKE-ACCOUNT " m)))))
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

; (def f
;   (let [a (atom 0)]
;     (defn g [x]
;       (let [b (atom @a)]
;         (swap! a + x)
;         @b))
;     g))

(defn f1 [value1]
   (defn f2 [value2]
     (let [value3 (atom value1)]
       (swap! value3 = value2)
       @value3))
   f2)
(defn f [x] ((f1 0) x))

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

; 3-12

; z makes a new list with x intact. first (cdr x) returns '(b)
; w replaces cdr pointer of the last pair of x to a pointer to y. second (cdr x) returns '(b c d)

; 3-13

; z forms a loop with three nodes having 'a, 'b and 'c
; (last-pair z) enters an infinite loop

; 3-14

; mystery returns a reversed list
; v => '(a)
; w => '(d c b a)

; 3-15

; z1 -> [ ][ ]
;        |  |
; x  -> [ ][ ] -> [ ][/]
;        |         |
;       wow        b

; z2 -> [ ][ ] -> [ ][ ] -> [ ][/]
;        |         |         |
;        |         a         b
;        |                   |
;       [ ][ ] -----------> [ ][/]
;        |
;       wow

; 3-16

; z3 -> [ ][ ] -> [ ][ ] -> [ ][/]
;        |         |         |
;        a         b         c

; z4 -> [ ][ ] -> [ ][ ] -> [ ][/]
;        |          \______/ |
;        a                   b

; z7 -> [ ][ ] -> [ ][ ] -> [ ][/]
;         \______/  \______/ |
;                            a

;        a         b         c
;        |         |         |
; z∞ -> [ ][ ] -> [ ][ ] -> [ ][ ]
;        \_____________________/

; 3-17

; doesn't really make sense as there's no clojure-equivalent of lisp's pair
; the general idea is to keep a set of visited pairs and count only those not in this set

; 3-18

; the algorithm used in 3-17 finds a cycle when a pair is already in the set of visited pairs

; 3-19

; keep two 'pointers', one of which scans the list one by one
; and the other proceeding at 2x speed of the former
; if there's a cycle, two pointers will ultimately coincide before reaching the end of the list

; 3-20

; (define x (cons 1 2))
; ; global <- E1 (x: 1, y: 2, set-x!: ..., set-y!: ..., dispatch: ...) [body of cons]

; (define z (cons x x))
; ; global <- E2 (x: x, y: x, ...) [body of cons]

; (set-car! (cdr z) 17)
; ; E2 <- E3 (m: 'cdr) [body of dispatch]
; ; global <- E4 (z: x, new-value: 17) [body of set-car!]
; ; E1 <- E5 (m: 'set-car!) [body of dispatch]
; ; E1 <- E6 (v: 17) [body of set-x!]
; ; global <- E1 (x: 17, y: 2, ...) [body of cons]

; (car x)
; ; E1 <- E7 (m: 'car) [body of dispatch]

; 3-21

(defn front-ptr [queue] (.car queue))
(defn rear-ptr [queue] (.cdr queue))
(defn set-front-ptr! [queue item] (.setcar queue item))
(defn set-rear-ptr! [queue item] (.setcdr queue item))

(defn empty-queue? [queue] (nil? (front-ptr queue)))
(defn make-queue [] (pair nil nil))

(defn front-queue [queue]
  (if (empty-queue? queue)
    (throw (Exception. (str "FRONT called with an empty queue" queue)))
    (.car (front-ptr queue))))

(defn insert-queue! [queue item]
  (let [new-pair (pair item nil)]
    (if (empty-queue? queue)
      (do
        (set-front-ptr! queue new-pair)
        (set-rear-ptr! queue new-pair)
        queue)
      (do
        (.setcdr (rear-ptr queue) new-pair)
        (set-rear-ptr! queue new-pair)
        queue))))

(defn delete-queue! [queue]
  (if (empty-queue? queue)
    (throw (Exception. (str "DELETE! called with an empty queue" queue)))
    (do
      (set-front-ptr! queue (.cdr (front-ptr queue)))
      queue)))

(defn print-queue [queue]
  (prn (reverse
    (loop [current-ptr (front-ptr queue) acc '()]
      (if (nil? current-ptr)
        acc
        (recur (.cdr current-ptr) (cons (.car current-ptr) acc)))))))

; 3-22

(defn make-queue []
  (let [front-ptr (atom nil)
        rear-ptr (atom nil)]
    (defn set-front-ptr! [item]
      (reset! front-ptr item))
    (defn set-rear-ptr! [item]
      (reset! rear-ptr item))
    (defn empty-queue? []
      (= nil @front-ptr))
    (defn front-queue []
      (if (empty-queue?)
        (throw (Exception. (str "FRONT called with an empty queue")))
        (.car @front-ptr)))
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
        (throw (Exception. (str "DELETE! called with an empty queue")))
        (set-front-ptr! (.cdr @front-ptr))))
    (defn print-queue []
      (prn (reverse
        (loop [current-ptr @front-ptr acc '()]
          (if (nil? current-ptr)
            acc
            (recur (.cdr current-ptr) (cons (.car current-ptr) acc)))))))
    (defn dispatch [m]
      (cond
        (= m 'empty-queue?) empty-queue?
        (= m 'front-queue) front-queue
        (= m 'insert-queue!) insert-queue!
        (= m 'delete-queue!) delete-queue!
        (= m 'print-queue) print-queue))
    dispatch))

; 3-28

(declare after-delay)
(declare inverter-delay)
(declare and-gate-delay)
(declare or-gate-delay)
(declare get-signal)
(declare set-signal!)
(declare add-action!)

(defn logical-not [s]
  (if (zero? s) 1 0))

(defn inverter [input output]
  (defn invert-input []
    (let [new-value (logical-not (get-signal input))]
      (after-delay
        inverter-delay
        (fn [] (set-signal! output new-value)))))
  (add-action! input invert-input))

(defn logical-and [s v]
  (if
    (and (= 1 s) (= 1 v)) 1
    0))

(defn and-gate [a1 a2 output]
  (defn and-action []
    (let [new-value (logical-and (get-signal a1) (get-signal a2))]
      (after-delay
        and-gate-delay
        (fn [] (set-signal! output new-value)))))
  (do
    (add-action! a1 and-action)
    (add-action! a2 and-action)))

(defn logical-or [s v]
  (if
    (or (= 1 s) (= 1 v)) 1
    0))

(defn or-gate [a1 a2 output]
  (defn or-action []
    (let [new-value (logical-or (get-signal a1) (get-signal a2))]
      (after-delay
        or-gate-delay
        (fn [] (set-signal! output new-value)))))
  (do
    (add-action! a1 or-action)
    (add-action! a2 or-action)))

; 3-29

(declare make-wire)

(defn or-gate-using-and-gate-inverter [a1 a2 output]
  (let [inverse-a1 (make-wire)
        inverse-a2 (make-wire)
        inverse-output (make-wire)]
    (do
      (inverter a1 inverse-a1)
      (inverter a2 inverse-a2)
      (and-gate inverse-a1 inverse-a2 inverse-output)
      (inverter inverse-output output))))

; total delay = 3 * inverter delay + and-gate delay

; 3-31

; initialization is necessary to capture the initial signal.
; if input procedure is not immediately run in half-adder,
; the initial output would be 0 no matter what input wires
; had at the time of the call to half-adder as make-wire assumes
; default signal of 0 when creating wires d and e.

; 3-33

(declare make-connector)
(declare adder)
(declare multiplier)
(declare constant)

(defn averager [a b c]
  (let [sum (make-connector)
        m (make-connector)]
    (adder a b sum)
    (constant 2 m)
    (multiplier c m sum)))

; 3-34

; multiplier assumes two of its three connectors to have values
; to set the value of the third. so setting the value of b alone
; can't compute the value of a.

; 3-35

(declare set-value!)
(declare has-value?)
(declare get-value)
(declare forget-value!)

(defn squarer [a b]
  (declare me)
  (defn process-new-value []
    (cond
      (has-value? b)
        (if (< (get-value b) 0)
          (throw (Exception. (str "square less than 0 -- SQUARER" (get-value b))))
          (set-value! a (sqrt (get-value b)) me))
      (has-value? a)
        (set-value! b (expt (get-value a) 2))))
  (defn process-forget-value []
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (defn me [request]
    (cond
      (= request 'I-have-a-value) (process-new-value)
      (= request 'I-lost-my-value) (process-forget-value)
      :else (throw (Exception. (str "Unknown request -- SQUARER" request)))))
  me)

; 3-37

(defn c-minus [x y]
  (let [z (make-connector)]
    (adder y z x)
    z))

(defn c-multiply [x y]
  (let [z (make-connector)]
    (multiplier x y z)
    z))

(defn c-divide [x y]
  (let [z (make-connector)]
    (multiplier y z x)
    z))

(defn cv [c]
  (let [z (make-connector)]
    (constant c z)
    z))

; 3-38

; a.
; peter, paul, mary: 45
; peter, mary, paul: 35
; paul, peter, mary: 45
; paul, mary, pater: 50
; mary, peter, paul: 40
; mary, paul, peter: 40

; b.
; suppose everyone reads at the same time the balance of 10,
; but peter's transaction completes last. then the balance is 110.

; 3-39

; P1 executes and then P2: 101
; P2 executes and then P1: 121
; serialized lambda inside P1 executes and then P2 and then P1: 100

; 3-40

; unserialized: 1000000, 1000, 100
; serialized: 1000000

; 3-41

; calling unserialized balance allows reading the balance while
; either withdraw or deposit is being executed. there's nothing wrong
; with that, though, as that reading is actually correct until any
; ongoing transaction gets completed. and since balance is read-only
; operation, it can't mess up outputs of other operations.

; 3-42

; this is a safe change. both versions provide the same concurrency.

; 3-44

; Ben's procedure should work fine.
; the difference between transfer and exchange is that the amount is
; known beforehand in the case of transfer, whereas in exchange the
; the amount is the difference between two account's balances

; 3-45

; (serialized-exchange a b)
; => ((s1 (s2 exchange)) a b)
; => ((s1 (s2 ((s1 withdraw) (s2 deposit)))) ...) ; not the exact expansion
; => s1 waits for s2 which waits for s1, resulting in a deadlock

; 3-46

; two procedures, namely A and B, call car-cell on the same mutex
; at the same time, both getting false and assuming the mutex is
; available. both then set the value of mutex to true.

; 3-48

; suppose a1 is the smaller-numbered account. then both Paul and Peter
; try to protect a1 first so either Paul waits Peter to finish or vice versa.

; 3-49

; for the given deadlock-avoidance mechanism to work, every procedure must
; know in advance all accounts it needs to access, to sort them by their
; numbers and decide in what order it should enter procedures protecting them.

; 3-50

; (defn stream-map [proc & argstreams]
;   (if (stream-null? (car argstreams))
;     the-empty-stream
;     (stream-cons
;       (proc (map stream-car argstreams))
;       (stream-map (cons proc (map stream-car argstreams))))))

; 3-51

; (def x (stream-map show (stream-enumerate-interval 0 10)))
; => 0
; (stream-ref x 5)
; => 1
; => 2
; => 3
; => 4
; => 5
; (stream-ref x 7)
; => 6
; => 7

; 3-53

; s is a list of powers of 2 starting from 1

; 3-54

(defn mul-streams [s1 s2]
  (stream-map * s1 s2))

(def factorials
  (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

; 3-55

(defn partial-sums [s]
  (cons-stream
    (stream-car s)
    (add-streams (partial-sums s) (stream-cdr s))))

; 3-56

(def S
  (cons-stream
    1
    (stream-merge
      (scale-stream S 2)
      (stream-merge
        (scale-stream S 3)
        (scale-stream S 5)))))

; 3-58

; expand performs division of num by den in the base of radix

; (expand 1 7 10) ; => (1 4 2 8 5 7 ...)
; (expand 3 8 10) ; => (3 7 5 0 0 0 ...)

; 3-59

(defn integrate-series [s]
  (stream-map / s (integers-starting-from 1)))

(defn cosine-series [s]
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(defn sine-series [s]
  (cons-stream 0 (integrate-series cosine-series)))

; 3-60

(defn mul-series [s1 s2]
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (add-streams
      (scale-stream (stream-cdr s1) (stream-car s2))
      (add-streams
        (scale-stream (stream-cdr s2) (stream-car s1))
        (mul-series (stream-cdr s1) (stream-cdr s2))))))

; 3-61

(defn invert-unit-series [s]
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (invert-unit-series s)) -1))

; 3-62

(defn div-series [nums dens]
  (let [den (stream-car dens)
        unit-dens (scale-stream dens (/ 1 den))]
    (if (zero? den)
      (throw (Exception. (str "denominator cannot be zero -- DIV-SERIES")))
      (scale-stream
        (mul-series nums (invert-unit-series unit-dens))
        (/ 1 den))))

(def tan-series
  (div-series sine-series cosine-series))
