(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 3-73
(defn RC [R C dt]
  (fn [i v0]
    (add-streams (integral (scale-stream ones (/ 1.0 C)) v0 dt)
                 (scale-stream i R))))