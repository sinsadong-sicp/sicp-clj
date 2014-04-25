(ns sicp-clj.ch2.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only []]))

; 2-78

 (defn type-tag [data]
   (cond (number? data) data
         (coll? data) (first data)
         :else (prn "error: type-tag")))

 (defn contents [data]
   (cond (number? data) data
         (coll? data) (second data)
         :else (prn "error: contents")))

 (defn attach-tag [tag content]
   (cond (number? content) content
         (coll? content) (cons tag content)))
