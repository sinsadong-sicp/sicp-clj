(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 2.4로 넘어감

; ; tagging
; (define (attach-tag type-tag contents)
;   (cons type-tag contents))
; (define (type-tag datum)
;   (if (pair? datum)
;       (car datum)
;       (error "Bad tagged datum -- TYPE-TAG" datum)))
; (define (contents datum)
;   (if (pair? datum)
;       (cdr datum)
;       (error "Bad tagged datum -- CONTENTS" datum)))
; (define (rectangular? z)
;   (eq? (type-tag z) 'rectangular))
; (define (polar? z)
;   (eq? (type-tag z) 'polar))

; ; Ben
; (define (real-part-rectangular z) (car z))
; (define (imag-part-rectangular z) (cdr z))
; (define (magnitude-rectangular z)
;   (sqrt (+ (square (real-part-rectangular z))
;            (square (imag-part-rectangular z)))))
; (define (angle-rectangular z)
;   (atan (imag-part-rectangular z)
;         (real-part-rectangular z)))
; (define (make-from-real-imag-rectangular x y)
;   (attach-tag 'rectangular (cons x y)))
; (define (make-from-mag-ang-rectangular r a)
;   (attach-tag 'rectangular
;               (cons (* r (cos a)) (* r (sin a)))))

; ; Alyssa
; (define (real-part-polar z)
;   (* (magnitude-polar z) (cos (angle-polar z))))
; (define (imag-part-polar z)
;   (* (magnitude-polar z) (sin (angle-polar z))))
; (define (magnitude-polar z) (car z))
; (define (angle-polar z) (cdr z))
; (define (make-from-real-imag-polar x y)
;   (attach-tag 'polar
;                (cons (sqrt (+ (square x) (square y)))
;                      (atan y x))))
; (define (make-from-mag-ang-polar r a)
;   (attach-tag 'polar (cons r a)))

; ; generic
; (define (real-part z)
;   (cond ((rectangular? z)
;          (real-part-rectangular (contents z)))
;         ((polar? z)
;          (real-part-polar (contents z)))
;         (else (error "Unknown type -- REAL-PART" z))))
; (define (imag-part z)
;   (cond ((rectangular? z)
;          (imag-part-rectangular (contents z)))
;         ((polar? z)
;          (imag-part-polar (contents z)))
;         (else (error "Unknown type -- IMAG-PART" z))))
; (define (magnitude z)
;   (cond ((rectangular? z)
;          (magnitude-rectangular (contents z)))
;         ((polar? z)
;          (magnitude-polar (contents z)))
;         (else (error "Unknown type -- MAGNITUDE" z))))
; (define (angle z)
;   (cond ((rectangular? z)
;          (angle-rectangular (contents z)))
;         ((polar? z)
;          (angle-polar (contents z)))
;         (else (error "Unknown type -- ANGLE" z))))

; ; operations
; (define (make-from-real-imag x y)
;   (make-from-real-imag-rectangular x y))
; (define (make-from-mag-ang r a)
;   (make-from-mag-ang-polar r a))

; (define (add-complex z1 z2)
;   (make-from-real-imag (+ (real-part z1) (real-part z2))
;                        (+ (imag-part z1) (imag-part z2))))
; (define (sub-complex z1 z2)
;   (make-from-real-imag (- (real-part z1) (real-part z2))
;                        (- (imag-part z1) (imag-part z2))))
; (define (mul-complex z1 z2)
;   (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;                      (+ (angle z1) (angle z2))))
; (define (div-complex z1 z2)
;   (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;                      (- (angle z1) (angle z2))))

; 오호라.. 패키지라는게 생기는군.

; 2-73

; 2-73-a
; exp를 var로 미분하는데, exp가 number면 바로 0을 리턴하고, exp가 variable인데 exp가 var과 같으면 1, 아니면 0을 리턴한다.
; 둘 다 아니면 get을 써서 어떤 oprator와 operand로 미분할지를 테이블에서 가져온다.
; number?는 해당 exp가 number인지를 알아내는 건데, dispatch는 type에 따라 다른 operation을 실행하겠다는 거라서 안 맞는 것 같다.
; same-variable?은 variable?인지 알아내고 나서 실행하는 거라서 비슷하게, differentiate할 여지가 없는 것으로 보인다.
; 그러니까... type에 따라 differentiate하겠다는 게 dispatch인데 이것들은 타입이 뭔지 알아내는 predicate라서 안 되는 건가?

; 2-73-b
(def install-sum-mul-package
    (defn make-sum [a1 a2]
        (cond
            (=number? a1 0) a2
            (=number? a2 0) a1
            (and (number? a1) (number? a2)) (+ a1 a2)
            :else (list '+ a1 a2)))
    (defn addened [exp] (first exp))
    (defn augend [exp] (second exp))
    (defn deriv-sum [exp var]
        (make-sum
                (deriv (addend exp) var)
                (deriv (augend exp) var)))
    (put 'deriv '+ deriv-sum)

    (defn make-product [m1 m2]
        (cond (or (=number? m1 0) (=number? m2 0)) 0
            (=number? m1 1) m2
            (=number? m2 1) m1
            (and (number? m1) (number? m2)) (* m1 m2)
            :else (list '* m1 m2)))
    (defn multiplier [exp] (first exp))
    (defn multiplicand [exp] (second exp))
    (defn deriv-mul [exp var]
        (make-sum
                (make-product
                    (multiplier exp)
                    (deriv (multiplicand exp) var))
                (make-product
                    (deriv (multiplier exp) var)
                    (multiplicand exp))))
    (put 'deriv '* deriv-mul)
)

; 2-73-c -> 안함
; 2-73-d
; put할 때 순서만 바꾸면 될 것 같다.
