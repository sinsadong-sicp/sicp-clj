(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only [sqr]]))

; 3-37
(defn c- [x y]
    (let [z (make-connector)]
        (adder y z x)
        z))

(defn c* [x y]
    (let [z (make-connector)]
        (multiplier x y z)
        z))

(defn c/ [x y]
    (let [z (make-connector)]
        (multiplier y z x)
        z))

(defn cv [x]
    (let [z (make-connector)]
        (constant x z)
        z))
