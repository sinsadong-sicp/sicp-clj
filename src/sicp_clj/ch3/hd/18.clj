(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-18
(defn element-of-set? [x set]
   (cond (empty? set) false
         (= x (first set)) true
         :else (element-of-set? x (rest set))))

(defn cycle? [item]
    (let [seen (atom '())]
        (defn iter [x]
            (cond
                (empty? @seen) false
                (element-of-set? (first x) @seen) true
                :else
                    (do
                        (reset! seen (cons (first x) @seen))
                        (iter (rest x)))))
        (iter item)))
