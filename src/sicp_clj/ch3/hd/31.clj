(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; 3-31
; accept-action-procedure는 add-action!을 했을 때 불리고, proc을 실행한다.
; add-action은 각각의 gate에서 or-action-procedure 같은 걸 proc으로 wire에 추가한다.
; or-action-procedure는 after-delay 함수를 안에 가지고 있다.
; after-delay는 add-to-agenda!를 불러서 the-agenda에 원소를 추가한다.
; propagate는 the-agenda에서 원소를 빼내 실행한다.
; 따라서 accept-action-procedure에 (proc)이 없으면 the-agenda에 원소가 추가되지 않기 때문에, propagte를 해도 아무것도 실행되지 않는다.
