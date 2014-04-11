(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only [sqr]]))

(defn make-pair-sum [pair]
    (list (first pair) (second pair) (+ (first pair) (second pair))))

; 2-40
(defn unique-pairs [n]
    (mapcat
        (fn [i] (map (fn [j] (list i j)) (range 1 i)))
        (range 1 (inc n))))

(defn prime-sum-pairs [n]
    (map make-pair-sum (filter prime-sum? (unique-pairs n))))

; 2-41
; unique-triples를 계속 못 만들어서 고민하다가, 만들어놓은 unique-pairs에 붙이는 걸로 해결.
; 웅철이 걸 보고 flatmap이 matcat으로 구현되어있다는 걸 알았다. 코드가 간단해졌군.
(defn triple-sum [n sum]
    (defn equal-sum? [triple]
        (== sum (+ (first triple) (second triple) (last triple))))
    (defn make-triple-sum [triple]
        (list (first triple) (second triple) (last triple)
            (+ (first triple) (second triple) (last triple))))
    (defn concat-distinct [pair]
        (map (fn [i] (list (first pair) i (second pair))) (range (inc (second pair)) (first pair))))
    (defn unique-triples [n]
        (mapcat (fn [i] (concat-distinct i)) (unique-pairs n)))
    (map make-triple-sum (filter equal-sum? (unique-triples n))))
