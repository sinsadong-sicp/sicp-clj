(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 2-79
; 각각의 package에 equ?를 put해주면 된다.
; scheme-number
; (put 'equ? '(scheme-number scheme-number) =) -> 파라미터 필요없이 그냥 비교하면 된다.

; rational
; (defn equ? [r1 r2]
;    (= (/ (numer r1) (denom r1)) (/ (numer r2) (denom r2))))
; (put 'equ? '(rational rational) equ?)

; complex
; (defn equ? [c1 c2]
;     (and (= (real-part c1) (real-part c2)) (= (imag-part c1) (imag-part c2))))
; (put 'equ? '(complex complex) equ?)

; 그리고 마지막으로
; (defn equ? [x y] (apply-generic 'equ? x y))

; 2-80
; scheme-number
; (put '=zero? '(scheme-number) (fn [x] (zero? x)))

; rational
; (defn =zero? [r]
;    (zero? (numer r))
; (put '=zero? '(rational) =zero?)

; complex
; (defn =zero? [c]
;     (and (zero? (real-part c)) (zero? (imag-part c))))
; (put '=zero? '(complex) =zero?)

; 그리고 마지막으로
; (defn =zero? [x] (apply-generic '=zero? x))

; 2-81
; a. apply-generic이 무한루프를 돈다. 계속해서 coerce할 것을 찾기 때문에.
; b. 그대로 놔둔다.
; c. apply-generic에서 type1과 type2를 let하는 곳 아래에 if 문으로 type1과 type2가 같으면 바로 error를 출력하도록 하는 코드를 추가한다.
