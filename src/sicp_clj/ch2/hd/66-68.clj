(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 2-66
; (defn lookup [given-key set-of-records]
;     (cond
;         (empty? set-of-records) false
;         (== given-key (entry set-of-records)) true
;         (< given-key (entry set-of-records))
;             (lookup given-key (left-branch set-of-records))
;         (> given-key (entry set-of-records))
;             (lookup given-key (right-branch set-of-records))
;         )
;     )

; Huffman 관련 코드
(defn element-of-set? [x set]
   (cond (empty? set) false
         (= x (first set)) true
         :else (element-of-set? x (rest set))))

(defn make-leaf [symbol weight] (list 'leaf symbol weight))
(defn leaf? [object] (= (first object) 'leaf))
(defn symbol-leaf [x] (second x))
(defn weight-leaf [x] (nth x 2))

(defn symbols [tree]
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (nth tree 2)))
(defn weight [tree]
  (if (leaf? tree)
      (weight-leaf tree)
      (nth tree 3)))

(defn make-code-tree [left right]
  (list left
        right
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn left-branch [tree] (first tree))
(defn right-branch [tree] (second tree))

(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)
        :else (prn "bad bit -- CHOOSE-BRANCH" bit)))

(defn decode [bits tree]
  (defn decode-1 [bits current-branch]
    (if (empty? bits)
        '()
        (let [next-branch
               (choose-branch (first bits) current-branch)]
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (rest bits) tree))
              (decode-1 (rest bits) next-branch)))))
  (decode-1 bits tree))

(defn adjoin-set [x set]
  (cond (empty? set) (list x)
        (< (weight x) (weight (first set))) (cons x set)
        :else (cons (first set)
                    (adjoin-set x (rest set)))))

(defn make-leaf-set [pairs]
  (if (empty? pairs)
      '()
      (let [pair (first pairs)]
        (adjoin-set (make-leaf (first pair)    ; symbol
                               (second pair))  ; frequency
                    (make-leaf-set (rest pairs))))))

; 2-67
; decode하면 (A D A B B C A)
(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; 2-68
(defn encode-symbol [sym tree]
    (defn my-branch? [branch]
        (element-of-set? sym (symbols branch)))
    (let [lb (left-branch tree) rb (right-branch tree)]
        (cond
            (my-branch? lb)
                (if (leaf? lb) '(0) (cons 0 (encode-symbol sym lb)))
            (my-branch? rb)
                (if (leaf? rb) '(1) (cons 1 (encode-symbol sym rb)))
            :else
                (prn "error: symbol not contained in the tree"))
        )
    )

(defn encode [message tree]
    (if (empty? message)
        '()
        (concat (encode-symbol (first message) tree)
                (encode (rest message) tree))))

(encode '(A D A B B C A) sample-tree) ; sample-message와 같게 나온다.

