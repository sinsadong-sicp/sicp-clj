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
      (if (= x 'how-many-calls?)
        @counter
        (do
          (swap! counter + 1)
          (f x))))))
