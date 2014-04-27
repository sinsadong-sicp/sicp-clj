(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-1
; atom과 swap이란 게 있구나. 이래서 클로저가 concurrent programming에 좋다는 건가?
(defn make-acc [value]
    (let [acc (atom value)]
        (fn [x] (swap! acc + x))))

; 3-2
(defn make-monitored [f]
    (let [call-count (atom 0)]
    (defn mf [msg]
        (cond
            (= msg 'how-many-calls?) @call-count
            (= msg 'reset-count) (reset! call-count 0)
            :else
                (do
                    (swap! call-count inc)
                    (f msg))))))
