(ns orbit.core
  (:gen-class))

(defn numerator [n] (if (= (type n) clojure.lang.Ratio) (clojure.core/numerator n) n))
(defn denominator [n] (if (= (type n) clojure.lang.Ratio) (clojure.core/denominator n) 1))

(defn sqrt-n [S]
  (letfn [(abs [n] (if (neg? n) (- n) n))
    (sqrt-r [x]
      (if (>= x (abs (- S (*' x x))))
        x
        (let [v (/ (+ x (/ S x)) 2)
              n (numerator v)
              d (denominator v)
              vr (+ (quot n d) (if (< (/ d 2) (rem n d)) 1 0))]
        (recur vr))))]
    (sqrt-r 2)))

(def G 1N)

(defn planet-acceleration [p1 p2]
  (if (= p1 p2) p1
    (let [dx (- (:x p2) (:x p1))
          dy (- (:y p2) (:y p1))
          dz (- (:z p2) (:z p1))
          d-sqr (+ (*' dx dx) (*' dy dy) (*' dz dz))
          a (/ (* G (:mass p2)) d-sqr)
          d (sqrt-n d-sqr)
          ax (* a (/ dx d))
          ay (* a (/ dy d))
          az (* a (/ dz d))
          ]
      {:ax ax :ay ay :az az})))

(defn compute-acceleration [planets]
  (map (fn [p] (apply merge-with + (map #(planet-acceleration p %) planets))) planets))

(defn compute-velocity [dt planets]
  (map (fn [p] (assoc p
                      :vx (+ (:vx p) (* (:ax p) dt))
                      :vy (+ (:vy p) (* (:ay p) dt))
                      :vz (+ (:vz p) (* (:az p) dt)))) planets))

(defn update-location [dt planets]
  (map (fn [p] (assoc p
                      :x (+ (:x p) (* (:vx p) dt))
                      :y (+ (:y p) (* (:vy p) dt))
                      :z (+ (:z p) (* (:vz p) dt)))) planets))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
