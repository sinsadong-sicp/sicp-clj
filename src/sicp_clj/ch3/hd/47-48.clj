(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-47
; (define (make-mutex)
;   (let ((cell (list false)))
;     (define (the-mutex m)
;       (cond ((eq? m 'acquire)
;              (if (test-and-set! cell)
;                  (the-mutex 'acquire))) ; retry
;             ((eq? m 'release) (clear! cell))))
;     the-mutex))
; (define (clear! cell)
;   (set-car! cell false))

; (define (test-and-set! cell)
;   (if (car cell)
;       true
;       (begin (set-car! cell true)
;              false)))
;a - mutex 써서 semaphore 만들기
(defn make-sema [n]
    (let [mutex (make-mutex) cnt (atom 0)]
        (defn the-sema [m]
            (cond
                (= m 'acquire)
                    (mutex 'acquire)
                    (if (= cnt n)
                        (do
                            (mutex 'release)
                            (the-sema 'acquire)))
                        (do
                            (reset! cnt inc)
                            (mutex 'release))
                (= m 'release)
                    (mutex 'acquire)
                    (if (> cnt 0)
                        (reset! cnt inc))
                    (mutex 'release)))
        the-sema))

;b - test-and-set! 써서 만들기 (clear!와 test-and-set!이 둘 다 atomic이라는 가정 필요.)
(defn make-sema [n]
    (let [cell (list false) cnt (atom 0)]
        (defn the-sema [m]
            (cond
                (= m 'acquire)
                    (if (test-and-set! cell)
                        (the-sema 'acquire)
                        (if (= cnt n)
                            (do
                                (clear! cell)
                                (the-sema 'acquire))
                            (do
                                (reset! cnt inc)
                                (clear! cell))))
                (= m 'release)
                    (if (test-and-set! cell)
                        (the-sema 'release)
                        (if (> cnt 0)
                            (reset! cnt dec)
                        (clear! cell))))
            )
        the-sema))

; 3-48
; 폴과 피터가 a1과 a2를 exchange한다고 하자. 더 작은 번호의 계정을 먼저 acquire해야 하기 때문에 폴은 a1을 acquire할 것이다. 그러면 피터는 a1을 acquire할 수 없기 때문에 애초에 exchange를 시작하지 못한다. 폴은 a2가 acquire되어있지 않으니까 exchange가 가능해진다. 그 반대도 마찬가지다.

; make-account 고치기 - 책에 나온 것을 고치는 형태로 한다.
; 두 account-id가 같을 때는 해결하지 못하는 형태이긴 한데.. 같은 id가 부여되는 걸 방지하려면 어떻게 해야하는지는 모르겠다.
(define (make-account balance account-id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) account-id)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (serialize-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (account1 'id))
        (id2 (account2 'id)))
    (if (< id1 id2)
        ((serializer2 (serializer1 exchange))
         account2
         account1)
        ((serializer1 (serializer2 exchange))
         account1
         account2))))
