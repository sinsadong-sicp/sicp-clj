(ns sicp-clj.ch4.wy)

; 4-1

(defn list-of-values-ltr [exps env]
  (if (no-operands? exps)
    nil
    (let [first-value (eval (first-operand exps) env)]
      (cons first-value (list-of-values-ltr (rest-operands exps) env)))))

(defn list-of-values-rtl [exps env]
  (if (no-operands? exps)
    nil
    (let [rest-values (list-of-values-rtl (rest-operands exps) env)]
      (cons (eval (first-operand exps) env) rest-values))))
