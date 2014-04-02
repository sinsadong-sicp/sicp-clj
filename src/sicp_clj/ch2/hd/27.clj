(ns sicp-clj.ch2.hd
  (:use [clojure.contrib.math :only []])
  (:use [clojure.contrib.generic.math-functions :only []]))

; 2-27
(defn deep-reverse [li]
    (if (vector? li)
        (reverse (map deep-reverse li))
        li))
