(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))
    
; 3-20
; 참고
;(define (cons x y)
;  (define (set-x! v) (set! x v))
;  (define (set-y! v) (set! y v))
;  (define (dispatch m)
;    (cond ((eq? m 'car) x)
;          ((eq? m 'cdr) y)
;          ((eq? m 'set-car!) set-x!)
;          ((eq? m 'set-cdr!) set-y!)
;          (else (error "Undefined operation -- CONS" m))))
;  dispatch)
;(define (car z) (z 'car))
;(define (cdr z) (z 'cdr))
;(define (set-car! z new-value)
;  ((z 'set-car!) new-value)
;  z)
;(define (set-cdr! z new-value)
;  ((z 'set-cdr!) new-value)
;  z)

; 아래 명령들을 순차적으로 불렀을 때 환경은 어떻게 되는가?
; (define x (cons 1 2))
; (define z (cons x x))
; (set-car! (cdr z) 17)
; (car x)

; cons, car, cdr, set-car!, set-cdr!, x, z가 전부 global env에 정의된다.
; x는 환경 E1, z는 환경 E2를 만든다.
; set-car!를 부르기 전까지, E1에서 x는 1 y는 2이고 E2에서 x는 x, y는 x다.
; 이제 set-car!에서, (cdr z)를 부르면 E2에서 (dispatch 'cdr)이 불리면서 환경 E3을 만든다. 여기서 m은 'cdr이 된다.
; (cdr z)는 y이므로, 결과적으로 (set-car! y 17)를 부르게 되는데, 이것이 E1에서 (dispatch 'set-car!)를 부르면서 E4를 만든다. 이 결과는 set-x!다.
; (set-x! 17)은 E1에서 E5를 만들고, E1의 x 값을 17로 바꾼다.
; 마지막으로 (car x)는 (dispatch 'car)를 E1에서 부르면서 E6를 만들고, 그 결과는 E1의 x인 17이다.

; 3-21
; (define (front-ptr queue) (car queue))
; (define (rear-ptr queue) (cdr queue))
; (define (set-front-ptr! queue item) (set-car! queue item))
; (define (set-rear-ptr! queue item) (set-cdr! queue item))
; (define (empty-queue? queue) (null? (front-ptr queue)))
; (define (make-queue) (cons '() '()))

; (define (front-queue queue)
;   (if (empty-queue? queue)
;       (error "FRONT called with an empty queue" queue)
;       (car (front-ptr queue))))

; (define (insert-queue! queue item)
;   (let ((new-pair (cons item '())))
;     (cond ((empty-queue? queue)
;            (set-front-ptr! queue new-pair)
;            (set-rear-ptr! queue new-pair)
;            queue)
;           (else
;            (set-cdr! (rear-ptr queue) new-pair)
;            (set-rear-ptr! queue new-pair)
;            queue)))) 
           
; (define (delete-queue! queue)
;   (cond ((empty-queue? queue)
;          (error "DELETE! called with an empty queue" queue))
;         (else
;          (set-front-ptr! queue (cdr (front-ptr queue)))
;          queue))) 

; 걍 이렇게 하면 될 것 같은데.. 지금 노트북에 뭐가 하나도 안깔려있어서 테스트하기 어렵다.
(defn print-queue [q]
  (prn (first q)))
