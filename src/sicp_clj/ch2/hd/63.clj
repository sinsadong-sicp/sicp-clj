(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 2-63
; (define (tree->list-1 tree)
;   (if (null? tree)
;       '()
;       (append (tree->list-1 (left-branch tree))
;               (cons (entry tree)
;                     (tree->list-1 (right-branch tree))))))
; (define (tree->list-2 tree)
;   (define (copy-to-list tree result-list)
;     (if (null? tree)
;         result-list
;         (copy-to-list (left-branch tree)
;                       (cons (entry tree)
;                             (copy-to-list (right-branch tree)
;                                           result-list)))))

; a) 둘 다 같다.
; b) 위는 append 한것을 또 append해야 하는데 밑은 cons만 하므로 아래 것이 더 빠르다.
