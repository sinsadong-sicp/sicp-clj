(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only [abs gcd]])
  (:use [clojure.contrib.generic.math-functions :only [sqr sqrt]]))

; 2-2
(defn make-point [x y] [x y])
(defn x-point [p] (first p))
(defn y-point [p] (last p))

(defn make-segment [start end] [start end])
(defn start-segment [seg] (first seg))
(defn end-segment [seg] (last seg))

(defn avrg [a b] (/ (+ a b) 2.0))
(defn midpoint-x [seg]
  (avrg (x-point (start-segment seg)) (x-point (end-segment seg))))
(defn midpoint-y [seg]
  (avrg (y-point (start-segment seg)) (y-point (end-segment seg))))

(defn midpoint-segment [seg]
  (make-point (midpoint-x seg) (midpoint-y seg)))

(defn print-point [p]
  (println (str "(" (x-point p) ", " (y-point p) ")")))

(def p1 (make-point 2 4))
(def p2 (make-point 5 11))
(def seg (make-segment p1 p2))
(print-point (midpoint-segment seg)) ; (3.5, 7.5)

; 2-3
; 사각형이 볼록사각형이고, 선분이 p1 - p2 - p3 - p4 - p1 으로 이어지는 경우에만 성립함.
; 일반적인 사각형을 구해보려고 했으나, 점을 늘어놓은 순서에 따라, 또는 선분 순서에 따라 넓이/둘레를 구하는 공식이 달라질 수도 있다.
; 즉 오목다각형/볼록다각형, 그리고 선분끼리 교차하느냐 아니냐에 따라 수식이 달리진다. 그래서 여기서 그냥 멈춘다.
(defn length-segment [seg]
  (sqrt (+ (sqr (- (x-point (start-segment seg)) (x-point (end-segment seg))))
           (sqr (- (y-point (start-segment seg)) (y-point (end-segment seg)))))))

(defn make-tetra [p1 p2 p3 p4] [p1 p2 p3 p4])
(defn p1-tetra[tet] (first tet))
(defn p2-tetra[tet] (second tet))
(defn p3-tetra[tet] (nth tet 2))
(defn p4-tetra[tet] (last tet))

(defn circum-tetra [tet]
  (let [p1 (p1-tetra tet) p2 (p2-tetra tet) p3 (p3-tetra tet) p4 (p4-tetra tet)]
    (+ (length-segment (make-segment p1 p2))
       (length-segment (make-segment p2 p3))
      (length-segment (make-segment p3 p4))
      (length-segment (make-segment p4 p1)))))

(defn area-triangle [p1 p2 p3] ; 헤론의 공식. 더 쉬운 버전이 있으나 그냥 정의대로 한다.
  (let [a (length-segment (make-segment p1 p2))
    b (length-segment (make-segment p2 p3))
    c (length-segment (make-segment p3 p1))
    s (/ (+ a b c) 2.0)]
    (sqrt (* s (- s a) (- s b) (- s c))) ))

(defn area-tetra [tet]
  (+ (area-triangle (p1-tetra tet) (p2-tetra tet) (p3-tetra tet))
    (area-triangle (p1-tetra tet) (p4-tetra tet) (p3-tetra tet))))

(def tet (make-tetra [0 0] [5 0] [5 5] [10 0]))
(prn (area-tetra tet)) ; 37.5
