(ns sicp-clj.ch2.hy.hy
  (:refer-clojure)
  (:use [clojure.contrib.math :only [abs gcd expt sqrt]]))

(defn mycons [x y]
  (defn dispatch [m]
    (cond
      (= m 0) x
      (= m 1) y
      :else (throw (Exception. "Argument not 0 or 1 - CONS"))))
dispatch)

(defn car [z]
  (z 0))
(defn cdr [z]
  (z 1))
(defn average [x y]
  (/ (+ x y) 2))
(defn square [x]
  (* x x))

;2-1
(defn make-rat [n d]
  (let [g (gcd n d)]
    (if (or (and (>= n 0) (> d 0)) (and (< n 0) (< d 0)))
      (mycons (/ (abs n) g) (/ (abs d) g))
      (mycons (- (/ (abs n) g)) (/ (abs d) g))
      )))

;2-2
(defn make-segment [p1 p2]
  (mycons p1 p2))
(defn start-segment [s]
  (car s))
(defn end-segment [s]
  (cdr s))
(defn make-point [x1 y1]
  (mycons x1 y1))
(defn x-point [p]
  (car p))
(defn y-point [p]
  (cdr p))
(defn midpoint-segment [s]
  (mycons
    (average (x-point (start-segment s)) (x-point (end-segment s)))
    (average (y-point (start-segment s)) (y-point (end-segment s)))))
(defn print-point [p]
  (println "(" (x-point p) "," (y-point p) ")"))

;2-3
;lower left - upper right
(defn make-rect [p1 p2]
  (mycons p1 p2))
(defn lower-left [r]
  (car r))
(defn upper-right [r]
  (cdr r))
(defn area-rect [r]
  (let [width (abs (- (x-point (upper-right r)) (x-point (lower-left r))))]
  (let [height (abs (- (y-point (upper-right r)) (y-point (lower-left r))))]
  (* width height))))
(defn peri-rect [r]
  (let [width (abs (- (x-point (upper-right r)) (x-point (lower-left r))))]
  (let [height (abs (- (y-point (upper-right r)) (y-point (lower-left r))))]
  (sqrt (+ (square width) (square height))))))

;2-4
(defn cons2 [x y]
  (fn [m] (m x y)))
(defn car2 [z]
  (z (fn [p q] p)))
(defn cdr2 [z]
  (z (fn [p q] q)))

;2-5
(defn cons3 [a b]
  (* (expt 2 a) (expt 3 b)))
(defn car3 [z]
  (defn iter [result x]
    (if (= 0 (rem x 2))
      (iter (inc result) (/ x 2))
      result))
  (iter 0 z))
(defn cdr3 [z]
  (defn iter [result x]
    (if (= 0 (rem x 3))
      (iter (inc result) (/ x 3))
      result))
  (iter 0 z))

;2-6
;집합론에서 자연수 정의를 {}, {{}}, {{{}}, {}} 이런식으로 하던거 생각남...
(defn zero []
  (fn [f]
    (fn [x] (x))))
(defn add-1 [n]
  (fn [f]
    (fn [x] (f ((n f) x)))))
(defn one []
  (fn [f]
    (fn [x] (f x))))
(defn two []
  (fn [f]
    (fn [x] (f (f x)))))
(defn add [a b]
  (fn [f]
    (fn [x] ((a f) ((b f) x)))))

;2-7
(defn make-interval [a b]
  (mycons a b))
(defn lower-bound [z]
  (car z))
(defn upper-bound [z]
  (cdr z))
(defn add-interval [x y]
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))
(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
    p2 (* (lower-bound x) (upper-bound y))
    p3 (* (upper-bound x) (lower-bound y))
    p4 (* (upper-bound x) (upper-bound y))]
  (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
(defn div-interval [x y]
  (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))))

;2-8
(defn sub-interval [x y]
  (let [
     u (- (upper-bound x) (lower-bound y))
     l (- (lower-bound x) (upper-bound y))]
  (make-interval l u)))

;2-9
(defn width-interval [z]
  (/ (- (upper-bound z) (lower-bound z)) 2))

;2-10
(defn div-interval2 [x y]
  (if (= 0 (width-interval y))
    (throw (Exception. "divided by 0 span interval - div-interval2"))
    (div-interval x y)))

;2-11
; http://goo.gl/MrN0Xb 이것을 보라
(defn mul-interval2 [x y]
  (let [a (lower-bound x)
    b (upper-bound x)
    c (lower-bound y)
    d (upper-bound y)]
  (cond
    (and (> a 0) (> c 0)) (make-interval (* a c) (* b d))
    (and (> a 0) (< d 0)) (make-interval (* b c) (* a d))
    (and (< b 0) (> c 0)) (make-interval (* a d) (* b c))
    (and (< b 0) (< d 0)) (make-interval (* b d) (* a c))
    (and (< a 0) (> b 0) (> c 0)) (make-interval (* a d) (* b d))
    (and (< a 0) (> b 0) (< d 0)) (make-interval (* b c) (* a c))
    (and (> a 0) (< c 0) (> d 0)) (make-interval (* b c) (* b d))
    (and (< b 0) (< c 0) (> d 0)) (make-interval (* a d) (* b c))
    :else (make-interval (min (* a d) (* b c)) (max (* a c) (* b d)))
  )))

;2-12
(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))
(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))
(defn make-center-percent [c p]
  (make-center-width c (* c (/ p 100))))
(defn percent [i]
  (* (/ width center) 100))

;2-17
(defn last-pair [li]
  (let [backlist (rest li)]
    (if (empty? backlist)
      li
      (last-pair backlist))))

;2-18
(defn first-pair [li]
  (let [backlist (rest li)]
    (if (empty? backlist)
      nil
      (conj (first-pair (rest li)) (first li)))))
(defn reverse-list [li]
  (let [backlist (rest li)]
  (if (empty? backlist)
    li
    (concat (last-pair li) (reverse-list (first-pair li))))))

;2-20
(defn same-parity [& s]
  (let [firstelem (first s)]
  (defn check-parity [li result]
    (if (empty? li)
      result
      (if (= (rem (- firstelem (first li)) 2) 0)
        (check-parity (rest li) (conj result (first li)))
        (check-parity (rest li) result))))
  (check-parity (reverse-list s) nil)))

;2-21
(defn square-list1 [s]
  (if (empty? s)
    s
    (conj (square-list1 (rest s)) (square (first s)))))
(defn square-list2 [s]
  (map square s))

;2-23
(defn for-each [f s]
  (if (empty? s)
    s
    (do
      (f (first s))
      (for-each f (rest s))
    )))

;2-24
;(1 (2 (3 4)))
; 1  (2 (3 4))
;     2 (3 4)
;        3 4

;2-25
;hytest 참고

;2-26
(def x (list 1 2 3))
(def y (list 4 5 6))

;2-27
(defn deep-reverse [li]
  (if (empty? li)
    nil
    (let [backlist (rest li)]
    (if (empty? backlist)
      (if (list? (first li))
        (list (deep-reverse (first li)))
        li)
      (concat (deep-reverse (last-pair li)) (deep-reverse (first-pair li)))))))

;2-28
(defn fringe [x]
  (if (empty? x)
    nil
    (if (list? (first x))
      (apply list (concat (fringe (first x)) (fringe (rest x))))
      (apply list (concat (list (first x)) (fringe (rest x)))))))

;2-29
(defn make-mobile [l r]
  (list l r))
(defn make-branch [l s]
  (list l s))
(defn left-branch [m]
  (first m))
(defn right-branch [m]
  (first (rest m)))
(defn branch-length [b]
  (first b))
(defn branch-structure [b]
  (first (rest b)))
(defn total-weight [m]
  (defn weight-branch [b]
    (if (list? (branch-structure b))
      (total-weight (branch-structure b))
      (branch-structure b)))
  (+ (weight-branch (left-branch m)) (weight-branch (right-branch m))))

(defn balanced? [m]
  (defn torque [b]
    (* (branch-length b) (weight-branch b)))
  (if (list? m)
    (and
      (= (torque (left-branch m))
         (torque (right-branch m)))
      (balanced? (branch-structure (left-branch m)))
      (balanced? (branch-structure (right-branch m))))
    true))

;2-30
(defn append [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))
(defn square-tree [tree]
  (cond
    (number? tree) (* tree tree)
    (empty? tree) nil
    :else (cons
      (square-tree (first tree))
      (square-tree (rest tree)))))
(defn square-tree-map [tree]
  (map (fn [sub-tree]
    (if (list? sub-tree)
      (square-tree-map sub-tree)
      (* sub-tree sub-tree))) tree))
;2-31
(defn tree-map [f tree]
  (map (fn [sub-tree]
    (if (list? sub-tree)
      (square-tree-map sub-tree)
      (f sub-tree))) tree))
;2-32
(defn subsets [s]
  (if (empty? s)
    (list nil)
    (let [re (subsets (rest s))]
      (append
        re
        (map
          (fn [x] (cons (first s) x))
          re)))))

;2-33
(defn accumulate [op initial sequence]
  (if (empty? sequence)
    initial
    (op (first sequence)
        (accumulate op initial (rest sequence)))))
(defn myfilter [pred sequence]
  (cond
    (empty? sequence) nil
    (pred (first sequence)) (cons (first sequence) (myfilter pred (rest sequence))))
    :else (myfilter pred (rest sequence)))
(defn mymap [p sequence]
  (accumulate (fn [x y] (cons (p x) y)) nil sequence))
(defn myappend [seq1 seq2]
  (accumulate cons seq2 seq1))
(defn mylength [sequence]
  (accumulate (fn [x y] (inc y)) 0 sequence))

;2-34
(defn horner-eval [x coeff]
  (accumulate
    (fn [this-coeff higher-terms]
      (+ (* higher-terms x) this-coeff))
    0
    coeff))

;2-35
(defn count-leaves [t]
  (accumulate + 0
    (map
      (fn [x]
        (if (list? x)
          (count-leaves x)
          1))
      t)))

;2-36
(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    nil
    (cons (accumulate op init (map first seqs)) (accumulate-n op init (map rest seqs)))))

;2-37
(defn dot-product [v w]
  (accumulate + 0 (map * v w)))
(defn matrix-*-vector [m v]
  (map (fn [k] (dot-product k v)) m))
(defn transpose [mat]
  (accumulate-n cons nil mat))
(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [k] (matrix-*-vector cols k)) m)))

;2-38
(def fold-right accumulate)
(defn fold-left [op initial seqs]
  (defn iter [result res]
    (if (empty? res)
      result
      (iter (op result (first res)) (rest res))))
  (iter initial seqs))

;2-39
(defn rev [seqs]
  (fold-right (fn [x y]
    (append y (list x))) nil seqs))
(defn rev2 [seqs]
  (fold-left (fn [x y]
    (cons y x)) nil seqs))

;2-40
(defn flatmap [proc seqs]
  (accumulate append nil (map proc seq)))
(defn interval-cal [low high]
  (if (> low high)
    nil
    (cons low (interval-cal (+ low 1) high))))
(defn unique-pairs [n]
  (mapcat ;;이거 flatmap으로 하면 왜 에러나는지 나중에 찾아봐야겠다
    (fn [x] (map (fn [y] (list x y)) (range 1 x)))
    (range 1 (inc n))))
(defn make-pair-sum [pair]
  (list (first pair) (second pair) (+ (first pair) (second pair))))
(defn divides? [a b]
  (= (rem b a) 0))
(defn find-divisor [n test-divisor]
  (cond
    (> (square test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor n (+ test-divisor 1))))
(defn smallest-divisor [n]
  (find-divisor n 2))
(defn prime? [n]
  (= (smallest-divisor n) n))
(defn prime-sum? [pair]
  (prime? (+ (first pair) (second pair))))
(defn prime-sum-pairs [n]
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

;2-41
(defn ordered-triples-sum [n s]
  (filter (fn [li] (= (accumulate + 0 list) s))
    (mapcat
      (fn [x]
        (mapcat (fn [y]
          (map (fn [z] (list x y z))
            (range 1 (- y 1))))
        (range 1 (- x 1))))
      (range 1 n))))

;2-42
(comment
(def empty-board nil)
(defn queens [board-size]
  (defn queen-cols [k]
    (if (= k 0)
      (list empty-board)
      (filter
        (fn [positions] (safe? k positions))
        (flatmap
          (fn [rest-of-queens]
            (map (fn [new-row]
              (adjoin-position new-row k rest-of-queens))
            (range 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size)))

;2-53
(defn memq [item x]
  (cond
    (empty? x) false
    (= item (first x)) x
    :else (memq item (rest x))))

;2-54
(defn equal? [a b]
  (cond
    (and (empty? a) (empty? b)) true
    (and (list? a) (list? b)) (and (= (first a) (first b)) (equal? (rest a) (rest b)))
    (and (symbol? a) (symbol? b)) (= a b)
    :else false))

;2-55
(comment
   (car ''abracadabra)
 = (car '('abracadabra))
 = (car '(quote abracadabra))
 = quote)

;2-57, 58

(defn variable? [x]
  (symbol? x))
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))
(defn =number? [exp nu]
  (and (number? exp) (= exp nu)))

(defn make-sum [a1 a2]
  (cond
    (=number? a1 0) a2
    (=number? a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    :else (list '+ a1 a2)))
(defn make-product [m1 m2]
  (cond
    (or (=number? m1 0) (=number? m2 0)) 0
    (=number? m1 1) m2
    (=number? m2 1) m1
    (and (number? m1) (number? m2)) (* m1 m2)
    :else (list '* m1 m2)))
(defn make-exponentiation [e1 e2]
  (cond
    (=number? e2 0) 1
    (=number? e2 1) e1
    (and (number? e1) (number? e2)) (expt e1 e2)
    :else (list '** e1 e2)))

(defn sum? [x]
  (and (list? x) (= (first x) '+)))
(defn addend [s]
  (second s))
;(defn augend [s]
;  (second (rest s)))
(defn augend [s]
  (if (> (count s) 3)
    (make-sum (addend (rest s)) (augend (rest s)))
    (first (rest (rest s)))))
(defn product? [x]
  (and (list? x) (= (first x) '*)))
(defn multiplier [p]
  (second p))
;(defn multiplicand [p]
;  (second (rest p)))
(defn multiplicand [p]
  (if (> (count p) 3)
    (make-product (multiplier (rest p)) (multiplicand (rest p)))
    (first (rest (rest p)))))
(defn exponentiation? [x]
  (and (list? x) (= (first x) '**)))
(defn base [p]
  (second p))
(defn exponent [p]
  (second (rest p)))

(defn deriv [exp v]
  (cond
    (number? exp) 0
    (variable? exp) (if (same-variable? exp v) 1 0)
    (sum? exp) (make-sum (deriv (addend exp) v) (deriv (augend exp) v))
    (product? exp) (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) v))
                             (make-product (deriv (multiplier exp) v) (multiplicand exp)))
    (exponentiation? exp) (make-product
                            (make-product (exponent exp) (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
                            (deriv (base exp) v))
    :else (throw (Exception. "Unknown expression type - DERIV"))))

;2-59
(defn union-set [set1 set2]
  (cond
    (empty? set1) set2
    (empty? set2) set1
    (contains? set1 (first set2)) (union-set set1 (rest set2))
    :else (set (cons (first set2) (union-set set1 (rest set2))))))

;2-61
(defn adjoin-set [x s]
  (cond
    (nil? x) s
    (empty? s) (set (list x))
    (= x (first s)) s
    (< x (first s)) (cons x s)
    :else (cons (first s) (adjoin-set x (rest s)))))

;2-62
(defn union-set-ordered [set1 set2]
  (cond
    (empty? set1) set2
    (empty? set2) set1
    (= (first set1) (first set2)) (union-set-ordered set1 (rest set2))
    (> (first set1) (first set2)) (cons (first set2) (union-set-ordered set1 (rest set2)))
    :else (cons (first set1) (union-set-ordered (rest set1) set2))))

;2-63
(defn entry [tree]
  (first tree))
(defn tree-left-branch [tree]
  (first (rest tree)))
(defn tree-right-branch [tree]
  (first (rest (rest tree))))
(defn make-tree [entry l r]
  (list entry l r))
(defn treelist1 [tree]
  (println tree)
  (if (nil? tree)
    '()
    (append (treelist1 (tree-left-branch tree))
            (cons (entry tree)
                  (treelist1 (tree-right-branch tree))))))
(defn treelist2 [tree]
  (defn copy-to-list [tree result-list]
    (println tree " " result-list)
    (if (nil? tree)
      result-list
      (copy-to-list (tree-left-branch tree)
                    (cons (entry tree)
                          (copy-to-list
                            (tree-right-branch tree)
                            result-list)))))
  (copy-to-list tree '()))
; a) hytest에서 보듯 결과는 같음
; b) balanced tree 일때 left branch = right branch = total n / 2 인데 위에꺼는 append라서 O(n/2) for each step
; 아래는 O(1) for each step => 재귀 O계산해보면 nlogn과 n의 차이로 나타남

