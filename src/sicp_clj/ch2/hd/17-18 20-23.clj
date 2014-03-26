(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only []])
  (:use [clojure.contrib.generic.math-functions :only [sqr]]))

; 2-17
(defn last-pair [li]
    [(last li)])

; 2-18
; 책에 정의된 list-ref와 length, append를 이용한다.
; list-ref->nth, length->count, append->conj
(defn my-reverse [li]
    (defn iter [counter result]
         (if (zero? counter)
             (conj result (first li))
             (iter (dec counter) (conj result (nth li counter)))))
    (iter (dec (count li)) []))

; 2-20
; clojure에서는 . 대신 & 를 쓴다.
; filter를 쓰지 않고 풀어보자.
(defn same-parity [& s]
    (def test? (if (even? (first s)) even? odd?))
    (defn iter [counter result]
        (cond
            (== (count s) counter) result
            (test? (nth s counter)) (iter (inc counter) (conj result (nth s counter)))
            :else (iter (inc counter) result)))
    (iter 0 []))

; 2-21
; map은 가장 간단하게는 이런 식으로 쓸 수 있다.
; (map #(* %1 10) [1 2 3 4 5])
(defn sqr-list-1 [items]
    (if (nil? items) []
        (conj (sqr-list-1 (butlast items)) (sqr (last items)))))

(defn sqr-list-2 [items]
    (map sqr items))

; 2-22
; 처음에는 answer가 비어있다가, 최초의 car things가 맨 처음으로 들어오게 되고, 그 뒤에는 나머지에서 car things가 그 앞에 들어온다. 결과적으로 뒤집힌다.
; 바꾼 것도 제대로 작동 안한다. cons는 원래 리스트의 맨 앞에 원소를 더하는 것이므로, 결과가 리스트의 리스트의 리스트... 같이 될 것이다.

; 2-23
(defn for-each [f s]
    (if (empty? s) nil
        (do
            (f (first s))
            (for-each f (rest s)))
        ))
