(ns sicp-clj.ch3.pair)

(definterface IPair
  (car [])
  (cdr [])
  (setcar [v])
  (setcdr [v]))

(deftype Pair [^:volatile-mutable x ^:volatile-mutable y]
  IPair
  (car [this] x)
  (cdr [this] y)
  (setcar [this v] (set! x v) this)
  (setcdr [this v] (set! y v) this))

(defn pair [x y] (Pair. x y))
