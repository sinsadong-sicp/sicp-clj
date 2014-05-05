(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-17
; contains? 가 안 먹어서 element-of-set을 써야 함.
(defn element-of-set? [x set]
   (cond (empty? set) false
         (= x (first set)) true
         :else (element-of-set? x (rest set))))

(defn count-pairs [x]
    (let [auxilary (atom '())]
        (defn counter [y]
            (if (or (not (list? y)) (element-of-set? y @auxilary))
                0
                (do
                    (reset! auxilary (cons y @auxilary))
                    (+ (counter (first x)) (counter (second x)) 1)
                    )
                )
            )
        (counter x))
    )
; 어찌아찌 된 것 같은데.. 맞는지 확인하기가 좀 어렵네.
