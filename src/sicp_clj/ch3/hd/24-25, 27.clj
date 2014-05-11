(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-24
; 다른 것은 다 똑같고, make-table이 same-key?를 파라미터로 받아서, 그 안에 assoc 프로시저를 정의하면 된다.
; assoc 프로시저는 다른 건 다 같고 equal?만 same-key?로 바뀐다.

; 3-25
; same-key?는 안 쓴다고 치자. 귀찮으니 그냥 car, cdr 등을 쓴다.
; (def make-table
;   (let [local-table (list '*table*)]
;     (defn lookup [key-list]
;       (defn lookup-inner [keys table]
;         (let [subtable (assoc (car keys) (cdr table))]
;           (if subtable
;               (if (empty? (cdr keys))
;                   (cdr subtable)
;                   (lookup-inner (cdr keys) subtable))
;               false)))
;       (lookup-inner key-list local-table))
;     (defn insert! [key-list value]
;       (defn insert-entry [keys]
;         (if (empty? (cdr keys))
;             (cons (car keys) value)
;             (list (car keys) (make-entry (cdr keys)))))
;       (defn insert-inner [keys table]
;         (let [subtable (assoc (car keys) (cdr table))]
;           (if subtable
;               (if (empty? (cdr keys))
;                   (set-cdr! subtable value)
;                   (insert-inner (cdr keys) subtable))
;               (set-cdr! table
;                         (cons (insert-entry keys)
;                               (cdr table))))
;       (insert-inner key-list local-table))
;       'ok))
;     (defn dispatch [m]
;       (cond (= m 'lookup-proc) lookup
;             (= m 'insert-proc!) insert!
;             :else (prn "Unknown operation -- TABLE" m)))
;     dispatch))

; 3-27
; n번째 피보나치 수를 찾는데 n단계만에 가능한 이유는 한번 찾은 값을 다시 안 찾기 때문이다. (너무 자명한데??)
; memo-fib를 (memoize fib)로 정의하면 fib의 내부에서 (fib (- n 1))과 (fib (- n 2))가 그대로 불리기 때문에 원하는 대로 작동하지 않는다.
