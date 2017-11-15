(ns orbit.core
  (:gen-class))

(defn sqrt-n [S]
  (letfn [(abs [n] (if (neg? n) (- n) n))
    (sqrt-r [x]
      (if (> x (abs (- S (*' x x))))
        x
        (recur (bigint (/ (+ x (/ S x)) 2)))))]
    (sqrt-r (/ S 2))))

(def G 1N)

(defn compute-acceleration [p1 p2]
  (if (= p1 p2) p1
    (let [dx (- (:x p2) (:x p1))
          dy (- (:y p2) (:y p1))
          d-sqr (+ (*' dx dx) (*' dy dy))
          a (/ (* G (:mass p2)) d-sqr)
          d (sqrt-n d-sqr)
          ax (* a (/ dx d))
          ay (* a (/ dy d))
          ]
      {:ax ax :ay ay})))

(defn gravity-step [dt planets]
 (map (fn [p] (apply merge-with + (map #(compute-acceleration p %) planets))) planets))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
