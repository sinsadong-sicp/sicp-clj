(ns sicp-clj.ch2.wy
  (:use [clojure.contrib.math :only [abs expt gcd sqrt]])
  (:use [clojure.contrib.generic.math-functions :only [sgn]]))

; 2-1

(defn make-rational [n d]
  (let [g (gcd n d)
        s (* (sgn n) (sgn d))]
    (vector
      (* s (/ (abs n) g))
      (/ (abs d) g))))

; 2-2

(defn make-point [x y]
  (vector x y))
(defn x-point [p]
  (first p))
(defn y-point [p]
  (second p))

(defn make-segment [p q]
  (vector p q))
(defn start-of-segment [s]
  (first s))
(defn end-of-segment [s]
  (second s))

(defn midpoint-of-segment [s]
  (defn average [x y]
    (/ (+ x y) 2))
  (let [p (start-of-segment s)
        q (end-of-segment s)
        x (average (x-point p) (x-point q))
        y (average (y-point p) (y-point q))]
    (make-point x y)))

; 2-3

(defn length-of-segment [s]
  (let [p (start-of-segment s)
        q (end-of-segment s)]
    (sqrt
      (+
        (expt (- (x-point p) (x-point q)) 2)
        (expt (- (y-point p) (y-point q)) 2)))))

; bottom left point + top right point
(defn make-rectangle [p q]
  (vector p q))
(defn length-of-rectangle [r]
  (let [p (first r)
        q (second r)]
    (length-of-segment
      (make-segment
        p
        (make-point (x-point q) (y-point p))))))
(defn width-of-rectangle [r]
  (let [p (first r)
        q (second r)]
    (length-of-segment
      (make-segment
        p
        (make-point (x-point p) (y-point q))))))

(defn perimeter-of-rectangle [r]
  (* 2 (+ (length-of-rectangle r) (width-of-rectangle r))))

(defn area-of-rectangle [r]
  (* (length-of-rectangle r) (width-of-rectangle r)))

; 2-4
; (car (cons x y))
; => ((cons x y) (fn [p q] p))
; => ((fn [m] (m x y)) (fn [p q] p))
; => ((fn [p q] p) x y)
; => x

(defn cdr [z]
  (z (fn [p q] q)))

; 2-5

(defn cons-int [a b]
  (* (expt 2 a) (expt 3 b)))

(defn car-int [n]
  (if (odd? n)
    0
    (inc (car-int (/ n 2)))))

(defn cdr-int [n]
  (if (not (zero? (rem n 3)))
    0
    (inc (cdr-int (/ n 3)))))

; 2-6

(def zero
  (fn [f]
    (fn [x]
      x)))

(defn add-1 [n]
  (fn [f]
    (fn [x]
      (f ((n f) x)))))

(def one
  (fn [f]
    (fn [x]
      (f x))))

(def two
  (fn [f]
    (fn [x]
      (f (f x)))))

(defn add [m n]
  (fn [f] (comp (m f) (n f))))

; 2-7

(defn make-interval [x y]
  (vector x y))
(defn upper-bound [interval]
  (first interval))
(defn lower-bound [interval]
  (second interval))

(defn add-interval [x y]
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(defn multiply-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (lower-bound y))]
    (make-interval (max p1 p2 p3 p4) (min p1 p2 p3 p4))))

(defn divide-interval [x y]
  (let [inverse-of-y (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))]
    (multiply-interval x inverse-of-y)))

; 2-8

(defn subtract-interval [x y]
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))

; 2-17

(def car first)
(def cdr rest)
(def cadr (comp car cdr))
(def caddr (comp car cdr cdr))
(def cadddr (comp car cdr cdr cdr))
(def cddr (comp cdr cdr))

(defn last-pair [xs]
  (if (empty? (cdr xs))
    (list (car xs))
    (last-pair (cdr xs))))

; 2-18

(defn append [xs ys]
  (if (empty? xs)
    ys
    (cons (car xs) (append (cdr xs) ys))))

(defn rev [xs]
  (if (empty? xs)
    nil
    (append (rev (cdr xs)) (list (car xs)))))

; 2-20

(defn same-parity [& lst]
  (let [same-parity? (if (odd? (car lst)) odd? even?)]
    (defn iter [xs ys]
      (if (empty? ys)
        xs
        (if (same-parity? (car ys))
          (iter (append xs (list (car ys))) (cdr ys))
          (iter xs (cdr ys)))))
    (iter (list (car lst)) (cdr lst))))

; 2-21

(defn square-list-1 [lst]
  (defn iter [xs ys]
    (if (empty? ys)
      xs
      (iter
        (append xs (list (expt (first ys) 2)))
        (rest ys))))
  (iter nil lst))

(defn square-list-2 [lst]
  (map (fn [x] (expt x 2)) lst))

; 2-22
; the last item in 'items' is the last to be cons-ed to the front of 'answer' list.
; interchanging arguments makes the first argument to be of list type.

; 2-23

(defn foreach [f xs]
  (if (empty? xs)
    true
    (do
      (f (car xs))
      (foreach f (cdr xs)))))

; 2-25
; (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))) ; => 7
; (car (car (list (list 7)))) ; => 7
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))) ; => 7

; 2-26
; (def x (list 1 2 3))
; (def y (list 4 5 6))
; (append x y) ; => (1 2 3 4 5 6)
; (cons x y) ; => ((1 2 3) 4 5 6)
; (list x y) ; => ((1 2 3) (4 5 6))

; 2-27

(defn deep-reverse [xs]
  (if (empty? xs)
    nil
    (append
      (deep-reverse (cdr xs))
      (let [x (car xs)]
        (list (if (list? x)
                (deep-reverse x)
                x))))))

; 2-28

(defn fringe [xs]
  (if (empty? xs)
    nil
    (append
      (let [x (car xs)]
        (if (list? x)
          (fringe x)
          (list x)))
      (fringe (cdr xs)))))

; 2-29

(defn make-mobile [lft rgt]
  (vector lft rgt))

(defn make-branch [len struc]
  (vector len struc))

(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (second mobile))

(defn branch-length [branch]
  (first branch))

(defn branch-structure [branch]
  (second branch))

(declare total-weight)
(defn branch-weight [branch]
  (let [struc (branch-structure branch)]
    (if (number? struc)
      struc
      (total-weight struc))))

(defn total-weight [mobile]
  (+
    (branch-weight (left-branch mobile))
    (branch-weight (right-branch mobile))))

(declare balanced-mobile?)
(defn balanced-branch? [branch]
  (let [struc (branch-structure branch)]
    (if (number? struc)
      true
      (balanced-mobile? struc))))

(defn balanced-mobile? [mobile]
  (defn torque [branch]
    (* (branch-length branch) (branch-weight branch)))
  (let [lft (left-branch mobile)
        rgt (right-branch mobile)]
    (and
      (= (torque lft) (torque rgt))
      (balanced-branch? lft)
      (balanced-branch? rgt))))

; 2-30

(defn square-tree-without-map [tree]
  (if (empty? tree)
    nil
    (append
      (let [x (car tree)]
        (list (if (list? x)
                (square-tree-without-map x)
                (* x x))))
      (square-tree-without-map (cdr tree)))))

(defn square-tree [tree]
  (map
    (fn [x]
      (if (list? x)
        (square-tree x)
        (* x x)))
    tree))

; 2-31

(defn tree-map [f tree]
  (map
    (fn [x]
      (if (list? x)
        (tree-map f x)
        (f x)))
    tree))

; 2-32

(defn subsets [s]
  (if (empty? s)
    (list nil)
    (let [rst (subsets (cdr s))]
      (append
        rst
        (map (fn [x] (cons (car s) x)) rst)))))

; 2-33

(defn accumulate [op initial xs]
  (if (empty? xs)
    initial
    (op
      (car xs)
      (accumulate op initial (cdr xs)))))

(defn map-acc [p xs]
  (accumulate
    (fn [x y] (cons x y))
    nil
    (map p xs)))

(defn append-acc [xs ys]
  (accumulate cons ys xs))

(defn length-acc [xs]
  (accumulate (fn [x y] (inc y)) 0 xs))

; 2-34

(defn horner-eval [x cs]
  (accumulate
    (fn [c terms] (+ (* terms x) c))
    0
    cs))

; 2-35

(defn count-leaves-acc [tree]
  (accumulate
    +
    0
    (map (fn [x] (if (list? x) (count-leaves-acc x) 1)) tree)))

; 2-36

(defn accumulate-n [op initial xs]
  (if (empty? (car xs))
    nil
    (cons
      (accumulate op initial (map car xs))
      (accumulate-n op initial (map cdr xs)))))

; 2-37

(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map (fn [row] (dot-product row v)) m))

(defn transpose [m]
  (accumulate-n cons nil m))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [row] (matrix-*-vector cols row)) m)))

; 2-38

(def foldr accumulate)
(defn foldl [op initial xs]
  (defn iter [acc ys]
    (if (empty? ys)
      acc
      (iter
        (op acc (car ys))
        (cdr ys))))
  (iter initial xs))

; (foldr / 1 (list 1 2 3)) => (/ 1 (/ 2 (/ 3 1))) => 3/2
; (foldl / 1 (list 1 2 3)) => (/ 3 (/ 2 (/ 1 1))) => 1/6
; (foldr list nil (list 1 2 3)) => (list 1 (list 2 (list 3 nil))) => (1 (2 (3 nil)))
; (foldl list nil (list 1 2 3)) => (list 3 (list 2 (list 1 nil))) => (((nil 1) 2) 3)

; for foldr and foldl to yield the same result, op should be associative

; 2-39

(defn reverse-foldr [xs]
  (foldr (fn [x y] (append y (list x))) nil xs))

(defn reverse-foldl [xs]
  (foldl (fn [x y] (cons y x)) nil xs))

; 2-40

(defn unique-pairs [n]
  (mapcat
    (fn [i]
      (map
        (fn [j] (list i j))
        (range 1 i)))
    (range 1 (inc n))))

; 2-41

(defn unique-triples [n]
  (mapcat
    (fn [i]
      (mapcat
        (fn [j]
          (map
            (fn [k]
              (list i j k))
            (range 1 j)))
        (range 1 i)))
    (range 1 (inc n))))

(defn ordered-triples-of-sum [n s]
  (defn pred [triple]
    (= s (foldr + 0 triple)))
  (filter pred (unique-triples n)))

; 2-42

; represent a set of board positions as a list of pairs
(defn queens [board-size]
  (def empty-board nil)
  (defn adjoin-position [x y positions]
    (cons [x y] positions))
  (defn safe? [k positions] ; check if the first position is safe against the rest
    (let [p (car positions)
          px (first p)
          py (second p)]
      (empty?
        (filter
          (fn [q]
            (let [qx (first q)
                  qy (second q)]
              (or
                (= px qx)
                (= (abs (- px qx)) (abs (- py qy))))))
          (cdr positions)))))
  (defn queen-cols [k]
    (if (zero? k)
      (list empty-board)
      (filter
        (fn [positions] (safe? k positions))
        (mapcat
          (fn [rest-of-queens]
            (map
              (fn [new-row]
                (adjoin-position new-row k rest-of-queens))
              (range 1 (inc board-size))))
          (queen-cols (dec k))))))
  (queen-cols board-size))

; 2-43

; in the original implementation queen-cols is called once for each column.
; Reasoner's solution calls queen-cols board-size times for every column,
; turning it into tree-recursive procedure whose running time grows exponentially.
; it would take T^(board-size) to run.

; 2-53

(defn memq [item xs]
  (cond
    (empty? xs) false
    (= item (car xs)) xs
    :else (memq item (cdr xs))))

; (list 'a 'b 'c) ; => (a b c)
; (list (list 'george)) ; => ((george))
; (cdr '((x1 x2) (y1 y2))) ; => ((y1 y2))
; (cadr '((x1 x2) (y1 y2))) ; => (y1 y2)
; (vector? (car '(a short list))) ; => false
; (memq 'red '((red shoes) (blue socks))) ; => false
; (memq 'red '(red shoes blue socks)) ; => (red shoes blue socks)

; 2-54

(defn equal? [xs ys]
  (cond
    (and (empty? xs) (empty? ys)) true
    (= (car xs) (car ys)) (equal? (cdr xs) (cdr ys))
    :else false))

; 2-55

; the second quote gets interpreted as a literal quote
; (car ''abra)
; => (car '(quote abra)))
; => quote

; 2-56, 57

(defn variable? [e]
  (symbol? e))
(defn same-variable? [u v]
  (and (variable? u) (variable? v) (= u v)))
(defn =number? [e n]
  (and (number? e) (= e n)))

(defn make-sum [u v]
  (cond
    (=number? u 0) v
    (=number? v 0) u
    (and (number? u) (number? v)) (+ u v)
    :else (list '+ u v)))
(defn make-product [u v]
  (cond
    (or (=number? u 0) (=number? v 0)) 0
    (=number? u 1) v
    (=number? v 1) u
    (and (number? u) (number? v)) (* u v)
    :else (list '* u v)))
(defn make-exponentiation [b e]
  (cond
    (=number? e 0) 1
    (=number? e 1) b
    :else (list '** b e)))

(defn sum? [e]
  (and (list? e) (= (car e) '+)))
(defn addend [sum]
  (cadr sum))
(defn augend [sum]
  (accumulate make-sum 0 (cddr sum)))

(defn product? [e]
  (and (list? e) (= (car e) '*)))
(defn multiplier [product]
  (cadr product))
(defn multiplicand [product]
  (accumulate make-product 1 (cddr product)))

(defn exponentiation? [e]
  (and (list? e) (= (car e) '**)))
(defn base [exponentiation]
  (cadr exponentiation))
(defn exponent [exponentiation]
  (caddr exponentiation))

(defn deriv [exp var]
  (cond
    (number? exp) 0
    (variable? exp)
      (if (same-variable? exp var) 1 0)
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
        (make-product
          (exponent exp)
          (make-exponentiation
            (base exp)
            (dec (exponent exp))))
        (deriv (base exp) var))
    :else (throw (Exception. "unknown expression type -- DERIV"))
  ))

; 2-58

; ; infix form
; (defn make-sum [u v]
;   (cond
;     (=number? u 0) v
;     (=number? v 0) u
;     (and (number? u) (number? v)) (+ u v)
;     :else (list u '+ v)))
; (defn sum? [e]
;   (and (list? e) (= (cadr e) '+)))
; (defn addend [sum]
;   (car sum))
; (defn augend [sum]
;   (caddr sum))

; ; standard algabraic notation
;

; 2-59

(defn element-of-set? [x s]
  (cond
    (empty? s) false
    (= x (car s)) true
    :else (element-of-set? x (cdr s))))

(defn union-set [a b]
  (cond
    (empty? a) b
    (empty? b) a
    (element-of-set? (car a) b) (union-set (cdr a) b)
    :else (cons (car a) (union-set (cdr a) b))))

; 2-60

; element-of-set? doesn't need to change: O(n)

(defn adjoin-set-with-duplicates [x s] ; O(1) (better than O(n))
  (cons x s))

(defn union-set-allow-duplicates [a b] ; O(n) (better than O(n^2))
  (append a b))

; intersection-set doesn't need to change: O(n^2)

; 2-61

(defn adjoin-set-ordered-list [x s]
  (cond
    (empty? s) (list x)
    (< x (car s)) (cons x s)
    :else (cons (car s) (adjoin-set-ordered-list x (cdr s)))))

; 2-62

(defn union-set-ordered-list [a b]
  (cond
    (empty? a) b
    (empty? b) a
    (< (car a) (car b)) (cons (car a) (union-set-ordered-list (cdr a) b))
    :else (cons (car b) (union-set-ordered-list a (cdr b)))))

; 2-63
; a. both procedures should return the same result: (1 3 5 7 9 11)
; b. for every nodes tree->list-1 calls append, whereas tree->list-2 calls cons.
;    append has linear time complexity, so tree->list-2 has a smaller order of growth.

; 2-64
; a. the procedure divides a given list into two,
;    first half being mapped to the left subtree,
;    the entry in the middle (car of the latter half) to the node,
;    and the second half to the right subtree.
;    list->tree (list 1 3 5 7 9 11)) should return (5 (1 () (3)) (9 (7) (11)))
; b. for every nodes tree->list calls cons. its time complexity equals O(n)

; 2-65

(defn intersection-set-ordered-list [a b]
  (if (or (empty? a) (empty? b))
    nil
    (let [x (car a)
          y (car b)]
      (cond
        (= x y)
          (cons x (intersection-set-ordered-list (cdr a) (cdr b)))
        (< x y)
          (intersection-set-ordered-list (cdr a) b)
        :else
          (intersection-set-ordered-list a (cdr b))))))

(defn make-tree [entry lft rgt]
  (list entry lft rgt))

(defn entry [tree]
  (car tree))
(defn left-tree [tree]
  (cadr tree))
(defn right-tree [tree]
  (caddr tree))

(defn to-list [tree] ; tree->list-2 from 2-63
  (defn tree-to-list [t lst]
    (if (empty? t)
      lst
      (tree-to-list
        (left-tree t)
        (cons
          (entry t)
          (tree-to-list (right-tree t) lst)))))
  (tree-to-list tree nil))

(defn to-tree [lst] ; list->tree from 2-64
  (defn partial-tree [xs n]
    (if (zero? n)
      (cons nil xs)
      (let [lft-size (quot (dec n) 2)
            lft-result (partial-tree xs lft-size)
            lft-tree (car lft-result)
            lft-else (cdr lft-result)
            this-entry (car lft-else)
            rgt-size (- n (inc lft-size))
            rgt-result (partial-tree (cdr lft-else) rgt-size)
            rgt-tree (car rgt-result)
            rgt-else (cdr rgt-result)]
        (cons (make-tree this-entry lft-tree rgt-tree) rgt-else))))
  (car (partial-tree lst (length-acc lst))))

; to-list has O(n) time complexity
; to-tree has O(n) time complexity
; union-set, intersection-set with ordered lists have O(n) time complexity

(defn union-set-linear-growth [a b]
  (to-tree (union-set-ordered-list (to-list a) (to-list b))))

(defn intersection-set-linear-growth [a b]
  (to-tree (intersection-set-ordered-list (to-list a) (to-list b))))

; 2-66

(defn lookup [k records] ; assume records are pairs
  (if (empty? records)
    false
    (let [this-key (first (entry records))]
      (cond
        (= k this-key) (second (entry records))
        (< k this-key) (lookup k (left-tree records))
        :else (lookup k (right-tree records))))))

; 2-67

(ns sicp-clj.ch2.wy.huffman
  (:use [sicp-clj.ch2.wy :only [car cdr cadr caddr cadddr append element-of-set?]]))

(defn make-leaf [sym w]
  (list 'leaf sym w))

(defn leaf? [x]
  (= (car x) 'leaf))
(defn symbol-leaf [x]
  (cadr x))
(defn weight-leaf [x]
  (caddr x))

(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(defn make-code-tree [lft rgt]
  (list
    lft
    rgt
    (append (symbols lft) (symbols rgt))
    (+ (weight lft) (weight rgt))))

(defn left-branch [tree]
  (car tree))
(defn right-branch [tree]
  (cadr tree))

(defn decode [bits tree]
  (defn choose-branch [bit branch]
    (cond
      (= 0 bit) (left-branch branch)
      (= 1 bit) (right-branch branch)
      :else (throw (Exception. "bad bit -- CHOOSE-BRANCH"))))
  (defn decode-recur [bits branch]
    (if (empty? bits)
      nil
      (let [next-branch (choose-branch (car bits) branch)]
        (if (leaf? next-branch)
          (cons
            (symbol-leaf next-branch)
            (decode-recur (cdr bits) tree))
          (decode-recur (cdr bits) next-branch)))))
  (decode-recur bits tree))

(def sample-tree
  (make-code-tree
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; (decode sample-message sample-tree) ; => (A D A B B C A)

; 2-68

(defn encode [message tree]
  (defn next-branch? [sym branch]
    (element-of-set? sym (symbols branch)))
  (defn encode-symbol [sym tree]
    (let [lft (left-branch tree)
          rgt (right-branch tree)]
      (cond
        (next-branch? lft)
          (if (leaf? lft)
            '(0)
            (cons 0 (encode-symbol sym lft)))
        (next-branch? rgt)
          (if (leaf? rgt)
            '(1)
            (cons 1 (encode-symbol sym rgt)))
        :else (throw (Exception. "bad symbol -- ENCODE-SYMBOL")))))
  (if (empty? message)
    nil
    (append
      (encode-symbol (car message) tree)
      (encode (cdr message) tree))))

; (encode '(A D A B B C A) sample-tree) ; => (0 1 1 0 0 1 0 1 0 1 1 1 0)
