(ns sicp-clj.ch3.wy
  (:use sicp-clj.ch3.pair)
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
  (prn (str "(" (front-ptr queue) ")")))

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
      (prn (str "(" @front-ptr ")")))
    (defn dispatch [m]
      (cond
        (= m 'empty-queue?) empty-queue?
        (= m 'front-queue) front-queue
        (= m 'insert-queue!) insert-queue!
        (= m 'delete-queue!) delete-queue!
        (= m 'print-queue) print-queue))
    dispatch))
