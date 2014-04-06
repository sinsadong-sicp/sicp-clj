(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only []])
  (:use [clojure.contrib.generic.math-functions :only []]))

; enumerator->filter->map->accumulate의 신호 흐름이라는 컨셉이 재미있다.
; filter는 clojure에 이미 있고, accumulate는 reduce를 쓰면 된다.
; ... 가 아니구나. reduce와 accumulate가 미묘하게 다르다. 그냥 accm 정의하자.

; (define (map proc items)
;   (if (null? items)
;       nil
;       (cons (proc (car items))
            ; (map proc (cdr items)))))

(defn ftr [predicate sequence]
  (cond (empty? sequence) nil
        (predicate (car sequence))
        (cons (first sequence) (ftr predicate (rest sequence)))
        :else (ftr predicate (rest sequence))))

(defn accm [op initial sequence]
  (if (empty? sequence) initial
      (op (first sequence) (accm op initial (rest sequence)))))

; 2-33
(defn map-list [proc sequence]
    (accm
        (fn [x y]
            (cons (proc x) y))
        nil
        sequence))

(defn append-list [seq1 seq2]
    (accm cons seq2 seq1))

(defn length-list [sequence]
    (accm (fn [x y] (inc y)) 0 sequence))

; 2-34
(defn horner-eval [x coefficient-sequence]
    (accm
        (fn [this-coeff higher-terms]
            (+ (* higher-terms x) this-coeff))
        0
        coefficient-sequence))

; (horner-eval 2 (list 1 3 0 5 0 1)) -> 79

; 2-35
; (define (count-leaves x)
;   (cond ((null? x) 0)
;         ((not (pair? x)) 1)
;         (else (+ (count-leaves (car x))
;                  (count-leaves (cdr x))))))

(defn count-leaves [t]
    (accm + 0 (map
        (fn [x]
            (if (list? x) (count-leaves x) 1)) t)))
