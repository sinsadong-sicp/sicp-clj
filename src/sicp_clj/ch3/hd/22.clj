(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-22
(def make-queue
    (let [front-ptr (atom nil) rear-ptr (atom nil)]
        (defn set-front-ptr! [item] (reset! front-ptr item))
        (defn set-rear-ptr! [item] (reset! rear-ptr item))
        (def empty-queue? (empty? front-ptr))
        (def front-queue
            (if (empty-queue?)
                (prn "FRONT called with an empty queue")
                (first front-ptr)))
        (defn insert-queue! [item]
            (let [new-pair (cons item '())]
                (if (empty-queue?)
                    (do
                        (set-front-ptr! new-pair)
                        (set-rear-ptr! new-pair)
                        dispatch))
                    (do
                        (set-cdr! rear-ptr new-pair)
                        (set-rear-ptr! new-pair)
                        dispatch)))
        (def delete-queue!
            (if (empty-queue?)
                (prn "DELETE! called with an empty queue"))
                (do
                    (set-front-ptr! (cdr front-ptr))
                    dispatch))
        (def print-queue
            (prn front-ptr))
        (defn dispatch [action]
            (cond
                (= action 'empty-queue?) empty-queue?
                (= action 'front-queue) front-queue
                (= action 'insert-queue!) insert-queue!
                (= action 'delete-queue!) delete-queue!
                (= action 'print-queue) print-queue
                :else (prn "Unknown action -- MAKE-QUEUE" action)))
        dispatch))
