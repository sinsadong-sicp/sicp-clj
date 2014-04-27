(ns sicp-clj.ch3.wy)

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
                (swap! counter + 1)
                (f x))))))
