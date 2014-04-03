(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only []])
  (:use [clojure.contrib.generic.math-functions :only []]))

; 2-28
(defn fringe [x]
    (if (empty? x)
        nil
        (concat
            (if (list? (first x))
                (fringe (first x))
                (list (first x)))
            (fringe (rest x)))))

; 중요한 사실: (empty? 5) 같은 걸 하면 에러난다.... 젠장 이걸 몰랐다니.
; 즉 recur 돌릴 때 무조건 리스트 형태로 보내야 한다.
; (coll? [1 2 3 4]) true
; (list? [1 2 3 4]) false
; (set? [1 2 3 4]) false
; (map? [1 2 3 4]) false
; (seq? [1 2 3 4]) false
; (vector? [1 2 3 4]) true

; (cons 1 '()) = (1)
; 그러나 (cons '(1) '()) = ((1))
; (concat '(1) '()) = (1)
; 그러나 (concat 1 '()) = error

; 2-29
(defn make-mobile [l r] (list l r))
(defn make-branch [length structure] (list length structure))

(defn left-branch [mobile] (first mobile))
(defn right-branch [mobile] (second mobile))
(defn branch-length [br] (first br))
(defn branch-struct [br] (second br))

(defn total-weight [mob]
    (let [lbs (branch-struct (left-branch mob))
        rbs (branch-struct (right-branch mob))]
        (+
            (if (list? lbs)
                (total-weight lbs)
                lbs)
            (if (list? rbs)
                (total-weight rbs)
                rbs)
            )
        )
    )

(defn mob-bal? [mob]
    (let [lbs (branch-struct (left-branch mob))
        rbs (branch-struct (right-branch mob))
        lbl (branch-length (left-branch mob))
        rbl (branch-length (right-branch mob))
        ]
        (and
            (==
                (* (total-weight lbs) lbl)
                (* (total-weight rbs) rbl)
            )
            (if (list? lbs) (mob-bal? lbs) true)
            (if (list? rbs) (mob-bal? rbs) true)
        )
    )
)

(def br1 (make-branch 7 10))
(def br2 (make-branch 5 15))
(def mob1 (make-mobile br1 br2))
(def br3 (make-branch 10 mob1))
(def mob2 (make-mobile br1 br3))
