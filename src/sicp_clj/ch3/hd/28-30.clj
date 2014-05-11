(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-28
; 사실상 찾아 바꾸기로 or라고 되어있는 부분을 전부 or로 바꾸면 된다.
; (define (or-gate a1 a2 output)
;   (define (or-action-procedure)
;     (let ((new-value
;            (logical-or (get-signal a1) (get-signal a2))))
;       (after-delay or-gate-delay
;                    (lambda ()
;                      (set-signal! output new-value)))))
;   (add-action! a1 or-action-procedure)
;   (add-action! a2 or-action-procedure)
;   'ok)

; 3-29
; (or a b) = (not (and (not a) (not b)))니까

; (defn or-gate [a1 a2 output]
;     (def or-action-procedure
;         (let [not-a1 (make-wire) not-a2 (make-wire) temp (make-wire)]
;             (inverter a1 not-a1)
;             (inverter a2 not-a2)
;             (and-gate not-a1 not-a2 temp)
;             (inverter temp output)))
;     (add-action! a1 or-action-procedure)
;     (add-action! a2 or-action-procedure)
;     'ok)

; delay는 and-gate-delay + 2 * inverter-delay다.

; 3-30
; (defn ripple-carry-adder [A B S C]
;     (let [c-in (make-wire)]
;         (if (empty? (rest A))
;             (set-signal! c-in 0)
;             (ripple-carry-adder (rest A) (rest B) (rest S) c-in))
;         (full-adder (first A) (first B) c-in (first S) C)))

; 총 delay는
; full-adder-delay * n
; = (half-adder-delay * 2 + or-gate-delay) * n
; = ( (max(or-gate-delay, and-gate-delay + inverter-delay) + and-gate-delay) * 2 + or-gate-delay ) * n
