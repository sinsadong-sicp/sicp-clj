(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 2-66
; (defn lookup [given-key set-of-records]
;     (def current-key (key (entry set-of-records)))
;     (cond
;         (empty? set-of-records) false
;         (== given-key current-key) (val (entry set-of-records))
;         (< given-key current-key)
;             (lookup given-key (left-branch set-of-records))
;         (> given-key current-key)
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

; 2-69
(defn successive-merge [leaf-set]
    (if (== (count leaf-set) 1) (first leaf-set)
        (successive-merge
            (adjoin-set
                (make-code-tree (first leaf-set) (second leaf-set))
                (rest (rest leaf-set)))
            )
        )
    )

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

; 2-70
(def song-tree (generate-huffman-tree '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1))))

(def song '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))

(count (encode song song-tree)) ; 84
; fixed면 각각 3bit씩 들었을 테니까, (count song-tree) * 3 = 108

; 2-71
; min은 1, max는 n-1.
