(ns sicp-clj.ch3.hd
    (:use [clojure.contrib.math :only []])
    (:use [clojure.contrib.generic.math-functions :only [sqr]]))

; 3-41
; 별로 필요하지 않다. account reading이 critical할 필요는 없기 때문이다.

; 3-42
; 안전하다. 모든 게 같은 serializer에 의해 핸들링되기 때문.
; concurrency상의 차이는 딱히 없는데, 새 것은 함수가 불리기 전에 시리얼라이즈하고, 옛날 것은 withdraw나 deposit이 불릴 때 한다.

; 3-44
; 틀렸다. exchange는 서로에게 의존적인 두 개의 계정에 대한 정보를 가지고 있어야 하는데, trasnfer는 누가 자신한테 옮겨주는지 알 필요가 없다.
