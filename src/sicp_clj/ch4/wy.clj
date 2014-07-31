(ns sicp-clj.ch4.wy)

; 4-1

(defn list-of-values-ltr [exp env]
  (if (no-operands? exp)
    nil
    (let [first-value (eval (first-operand exp) env)]
      (cons first-value (list-of-values-ltr (rest-operands exp) env)))))

(defn list-of-values-rtl [exp env]
  (if (no-operands? exp)
    nil
    (let [rest-values (list-of-values-rtl (rest-operands exp) env)]
      (cons (eval (first-operand exp) env) rest-values))))

; 4-4

(defn eval-and [exp env]
  (if (no-operands? exp)
    true
    (if (false? (eval (first-operand exp) env))
      false
      (eval-and (eval (rest-operands exp) env)))))

(defn eval-or [exp env]
  (if (no-operands? exp)
    false
    (if (true? (eval (first-operand exp) env))
      true
      (eval-or (eval (rest-operands exp) env)))))
