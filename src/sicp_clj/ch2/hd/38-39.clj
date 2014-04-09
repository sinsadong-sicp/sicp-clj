(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only []])
  (:use [clojure.contrib.generic.math-functions :only []]))

; 2-38
(defn fold-right [op initial sequence]
    (if (empty? sequence) initial
        (op (first sequence) (fold-right op initial (rest sequence)))))

(defn fold-left [op initial sequence]
  (defn iter [result remain]
    (if (empty? remain)
        result
        (iter (op result (first remain)) (rest remain))))
  (iter initial sequence))

; (fold-right / 1 (list 1 2 3)) -> 3/2
; (fold-left / 1 (list 1 2 3)) -> 1/6
; (fold-right list nil (list 1 2 3)) -> (1 (2 (3)))
; (fold-left list nil (list 1 2 3)) -> (((1) 2) 3)

; (op a b) = (op b a)이면 fold-right와 fold-left가 같다.

; 2-39
(defn reverse-right [sequence]
  (fold-right (fn [x y] (concat y (list x))) nil sequence))
; (op 1 (op 2 (op 3 nil)))

(defn reverse-left [sequence]
  (fold-left (fn [x y] (cons y x)) nil sequence))
; (op (op (op nil 1) 2) 3)
