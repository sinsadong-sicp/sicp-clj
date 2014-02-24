(ns sicp-clj.ch1.hd)

; 1-22
(defn start-prime-test[n start-time]
	(defn report-prime[elapsed]
		(str n " *** " elapsed " msecs")
	)	
	(if (prime? n)
		(report-prime (/ (double (- (. System (nanoTime)) start-time)) 1000000.0)))
)

(defn timed-prime-test[n]	
	(start-prime-test n (. System (nanoTime)))
)

(defn search-for-prime [start end]
	(defn get-odds[]
		(if (even? start)
			(range (inc start) end 2)
			(range start end 2))
	)
	(def result (map timed-prime-test (get-odds)))	
	(take 3 (filter (complement nil?) result))
)
; 1000에서: 0.01 밀리초
; 10000에서: 0.03 밀리초
; 100000에서: 0.09 밀리초
; 1000000에서: 변동이 좀 있는데.. 약 0.27 밀리초
; sqrt(10)이 3.16 정도이므로, 대략 3배씩 늘어나는 시간이 그럴듯하게 보인다.