(ns sicp-clj.ch1.hd
  (:use clojure.contrib.generic.math-functions))

; 1-40
(def dx 0.00001)

(defn close-enough?[x y]
  (< (abs (- x y)) dx))

(defn fixed-point[f first-guess]
  (defn try-guess[guess]
    (let [next-val (f guess)]
      (if (close-enough? guess next-val)
        next-val (try-guess next-val))))
  (try-guess first-guess))

(defn deriv[g]
  (fn[x] (/ (- (g (+ x dx)) (g x)) dx)))

(defn newton-transform[g]
  (fn[x] (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method[g guess]
  (fixed-point (newton-transform g) guess))

(defn cubic[a b c]
  (fn[x] (+ (* x x x) (* a x x) (* b x) c)))

; 1-41
(defn double-fn[f]
  (fn[x] (f (f x))))

(prn (((double-fn (double-fn double-fn)) inc) 5)) ; 21

; 1-42
(defn compose[f g]
  (fn[x] (f (g x))))

(prn ((compose sqr inc) 6)) ; 49

; 1-43
(defn repeated[f n]
  (defn iter[result-fn counter]
    (if (= counter 1)
      result-fn
      (iter (compose f result-fn) (dec counter))))
  (iter f n))

(prn ((repeated sqr 2) 5))
