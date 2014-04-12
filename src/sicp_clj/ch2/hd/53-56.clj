(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only [pow]]))

; 2-53
(defn memq [item x]
    (cond
        (empty? x) false
        (= item (first x)) x
        :else (memq item (rest x))))

; (list 'a 'b 'c) -> (a b c)
; (list (list 'george)) -> ((george))
; (cdr '((x1 x2) (y1 y2))) -> ((y1 y2))
; (cadr '((x1 x2) (y1 y2))) -> (y1 y2)
; (pair? (car '(a short list))) -> false
; (memq 'red '((red shoes) (blue socks))) -> false
; (memq 'red '(red shoes blue socks)) -> (red shos blue socks)

; 2-54
(defn equal? [x y]
    (cond
        (and (symbol? x) (symbol? y)) (= x y)
        (and (list? x) (list? y)) (and (= (first x) (first y)) (equal? (rest x) (rest y)))
        :else false))

; 2-55
; quote 자체가 symbol로 취급되었으니까.

; 2-56
(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn variable? [x] (symbol? x))
(defn same-variable? [v1 v2] (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum [term]
    (cond
        (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
    (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

(defn sum? [x] (and (list? x) (= (first x) '+)))
(defn addend [s] (second s))
(defn augend [s] (last s))
(defn product? [x] (and (list? x) (= (first x) '*)))
(defn multiplier [p] (second p))
(defn multiplicand [p] (last p))

(defn exponentiation? [x] (and (list? x) (= (first x) '**)))
(defn base [e] (second e))
(defn exponent [e] (last e))
(defn make-exponentiation [base expo]
    (cond
        (=number? expo 0) 1
        (=number? expo 1) base
        (and (number? base) (number? expo)) (pow base expo)
        :else (list '** base expo)))

(defn deriv [exp var]
    (cond
        (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp)
            (make-sum
                (deriv (addend exp) var)
                (deriv (augend exp) var))
        (product? exp)
            (make-sum
                (make-product
                    (multiplier exp)
                    (deriv (multiplicand exp) var))
                (make-product
                    (deriv (multiplier exp) var)
                    (multiplicand exp)))
        (exponentiation? exp)
            (make-product
                (exponent exp)
                (make-product
                    (make-exponentiation
                        (base exp)
                        (make-sum (exponent exp) '-1))
                    (deriv (base exp) var)))
        :else (prn "unknown expression type -- DERIV" exp)))
