(ns sicp-clj.ch3.wy.ex3_23
  (:use sicp-clj.ch3.pair))

(defn front-ptr [deque] (.car deque))
(defn rear-ptr [deque] (.cdr deque))
(defn set-front-ptr! [deque item] (.setcar deque item))
(defn set-rear-ptr! [deque item] (.setcdr deque item))

(defn empty-deque? [deque] (nil? (front-ptr deque)))
(defn make-deque [] (pair nil nil))

(defn front-deque [deque]
  (if (empty-deque? deque)
    (throw (Exception. (str "FRONT called with an empty deque" deque)))
    (.car (front-ptr deque))))

(defn rear-deque [deque]
  (if (empty-deque? deque)
    (throw (Exception. (str "REAR called with an empty deque" deque)))
    (.car (rear-ptr deque))))

(defn front-insert-deque! [deque item]
  (let [new-pair (pair item (pair nil nil))]
    (if (empty-deque? deque)
      (do
        (set-front-ptr! deque new-pair)
        (set-rear-ptr! deque new-pair))
      (do
        (.setcdr (.cdr new-pair) (front-ptr deque))
        (.setcar (.cdr (front-ptr deque)) new-pair)
        (set-front-ptr! deque new-pair)))))

(defn rear-insert-deque! [deque item]
  (let [new-pair (pair item (pair nil nil))]
    (if (empty-deque? deque)
      (do
        (set-front-ptr! deque new-pair)
        (set-rear-ptr! deque new-pair))
      (do
        (.setcar (.cdr new-pair) (rear-ptr deque))
        (.setcdr (.cdr (rear-ptr deque)) new-pair)
        (set-rear-ptr! deque new-pair)))))

(defn front-delete-deque! [deque]
  (if (empty-deque? deque)
    (throw (Exception. (str "FRONT-DELETE! called with an empty deque" deque)))
    (let [new-front (.cdr (.cdr (front-ptr deque)))]
      (if (nil? new-front)
        (do
          (set-front-ptr! deque nil)
          (set-rear-ptr! deque nil))
        (do
          (.setcar (.cdr new-front) nil)
          (set-front-ptr! deque new-front))))))

(defn rear-delete-deque! [deque]
  (if (empty-deque? deque)
    (throw (Exception. (str "REAR-DELETE! called with an empty deque" deque)))
    (let [new-rear (.car (.cdr (rear-ptr deque)))]
      (if (nil? new-rear)
        (do
          (set-rear-ptr! deque nil)
          (set-front-ptr! deque nil))
        (do
          (.setcdr (.cdr new-rear) nil)
          (set-rear-ptr! deque new-rear))))))

(defn print-deque [deque]
  (prn
    (loop [current-ptr (rear-ptr deque) acc '()]
      (if (nil? current-ptr)
        acc
        (recur (.car (.cdr current-ptr)) (cons (.car current-ptr) acc))))))
