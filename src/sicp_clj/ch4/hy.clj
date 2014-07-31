(ns sicp-clj.ch4.hy.hy
  (:use sicp-clj.ch4.pair :reload)
  (:use [clojure.contrib.math :only [abs gcd expt sqrt]])
  (:refer-clojure))

(defn list-of-values-lr [exps env]
  (if (no-operands? exps)
    nil
    (let [left-value (eval (first-operand exps) env)]
         [right-value (eval (rest-operands exps) env)]
      (cons left-value right-value))))

(defn list-of-values-rl [exps env]
  (if (no-operands? exps)
    nil
    (let [left-value (eval (first-operand exps) env)]
         [right-value (eval (rest-operands exps) env)]
      (cons right-value left-value))))
