(ns schlang.vector)

(defn distance [[x1 y1] [x2 y2]]
  (let [dx-abs (js/Math.abs (- x2 x1))
        dy-abs (js/Math.abs (- y2 y1))]
    (js/Math.sqrt (+ (* dx-abs dx-abs)
                     (* dy-abs dy-abs)))))

(defn rad->degree [n]
  (/ (* 360 n)
     (* 2 js/Math.PI)))

(defn dot-product [[x1 y1] [x2 y2]]
  (+ (* x1 x2)
     (* y1 y2)))

(defn angle-rad [[x1 y1 :as a] [x2 y2 :as b]]
  (let [backwards (> x1 x2)
        c [x1 (inc y1)]
        [x3 y3] c
        a-c [(- x3 x1) (- y3 y1)]
        a-b [(- x2 x1) (- y2 y1)]
        angle (js/Math.acos (/ (dot-product a-b a-c)
                         (distance a c) (distance a b)))]
    (if backwards
      (- (* 2 js/Math.PI) angle)
      angle)))

(defn angle-degree [a b]
  (rad->degree (angle-rad a b)))

(defn unit-circle [a b]
  (let [angle (angle-rad a b)]
    [(js/Math.sin angle) (js/Math.cos angle)]))


(unit-circle [0 0] [1 1])