(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only []])
  (:use [clojure.contrib.generic.math-functions :only [sqr]]))

; 2-30
; 책에 나온 정의
; (define (scale-tree tree factor)
;   (cond ((null? tree) nil)
;         ((not (pair? tree)) (* tree factor))
;         (else (cons (scale-tree (car tree) factor)
;                     (scale-tree (cdr tree) factor)))))
; (define (scale-tree tree factor)
;   (map (lambda (sub-tree)
;          (if (pair? sub-tree)
;              (scale-tree sub-tree factor)
;              (* sub-tree factor)))
;        tree))

(defn sqr-tree-1 [t]
    (if (empty? t)
        nil
        (cons
            (if (list? (first t))
                (sqr-tree-1 (first t))
                (sqr (first t)))
            (sqr-tree-1 (rest t)))))

(defn sqr-tree-2 [t]
    (map
        (fn [sub-tree]
            (if (list? sub-tree)
                (sqr-tree-2 sub-tree)
                (sqr sub-tree)))
        t))

(def mytree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; 와, map 쓰니까 정말 편하군.

; 2-31
(defn tree-map [op tree]
    (map
        (fn [sub-tree]
            (if (list? sub-tree)
                (tree-map op sub-tree)
                (op sub-tree)))
        tree))

(defn sqr-tree [tree]
    (tree-map sqr tree))

; 2-32
(defn subsets [s]
  (if (empty? s)
    '(nil)
    (let [rt (subsets (rest s))]
        (concat rt (map (fn [st] (cons (first s) st)) rt)))))
