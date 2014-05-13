(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only [sqr]]))

; 3-33
(defn averager [a b c]
    (let [x (make-connector) y (make-connector)]
        (adder a b x)
        (multiplier c y x)
        (constant 2 y)
        'ok))

; 3-34
; b값만 세팅되었을 때 a를 구할 수 없다. 멀티플라이어에서 a * b = c일 때, b와 c가 전부 주어져야만 a를 알 수 있다. 그러나 a * a = b일 때 b가 세팅되더라도 나머지 하나가 세팅이 안 되기 때문에 a를 구할 수 없다.

; 3-35
; 귀찮아서 책에서 복사한 거 안바꾸고 대충 스킴 스타일 섞어서 함.
; (define (squarer a b)
;     (define (process-new-value)
;         (if (has-value? b)
;             (if (< (get-value b) 0)
;                 (error "square less than 0 -- SQUARER" (get-value b))
;                 (set-value! a (sqr (get-value b)) me))
;             (if (has-value? a)
;                 (set-value! b (square (get-value a)) me))))
;     (define (process-forget-value)
;         (forget-value! b me)
;         (forget-value! a me)
;         (process-new-value))
;     (define (me request)
;         (cond
;             (= request 'I-have-a-value) (process-new-value)
;             (= request 'I-lost-my-value) (process-forget-value)
;             :else (error "Unknown request -- SQUARER" request)))
;     (connect a me)
;     (connect b me)
;     me)
