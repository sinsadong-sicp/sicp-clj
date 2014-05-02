(ns sicp-clj.ch3.hy.hy
  (:refer-clojure))

;3-1
(defn make-accumulator [init]
  (let [sum (atom init)]
    (defn addition [addend]
      (swap! sum + addend)))
    addition)
