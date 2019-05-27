(ns orbit.core
  (:refer-clojure :exclude [numerator denominator])
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

(def G 1)

(defn pos
  [{:keys [x y z]}]
  [x y z])

(defn delta-pos
  [p1 p2]
  (mapv - (pos p1) (pos p2)))

(defn v
  [{:keys [vx vy vz]}]
  [vx vy vz])

(defn acc
  [{:keys [ax ay az]}]
  [ax ay az])

(defn d2
  [p1 p2]
  (->> (delta-pos p1 p2)
       (map #(* % %))
       (reduce +)))

(defn planet-acceleration
  [p1 p2]
  (if (= p1 p2)
    (assoc  p1 :ax 0 :ay 0 :az 0)
    (let [d-2 (d2 p1 p2)
          a (/ (* G (:mass p2)) d-2)
          d (sqrt-n d-2)
          a- (map #(* a (/ % d)) (delta-pos p2 p1))]
      (zipmap [:ax :ay :az] a-))))

(defn compute-acceleration
  [planets]
  (map (fn [p]
         (apply merge-with
                +
                (map #(planet-acceleration p %)
                     planets)))
       planets))

(defn compute-velocity-t
  [dt]
  (fn [rf]
    (fn ([] (rf))
      ([result] (rf result))
      ([result p]
       (rf result (-> p
                      (update :vx (partial + (* (:ax p 0) dt)))
                      (update :vy (partial + (* (:ay p 0) dt)))
                      (update :vz (partial + (* (:az p 0) dt)))))))))

(defn compute-position-t
  [dt]
  (fn [rf]
   (fn ([] (rf))
     ([result] (rf result))
     ([result p]
      (rf result (-> p
                     (update :x (partial + (* (:vx p 0) dt)))
                     (update :y (partial + (* (:vy p 0) dt)))
                     (update :z (partial + (* (:vz p 0) dt)))))))))

(defn planet-line [planet]
  (letfn [(scale [n] (+ 250 (Math/round (/ n 1000.0))))]
    (str "<circle cx=\"" (scale (:x planet)) "\" cy=\"" (scale (:y planet)) "\" r=\"5\" fill=\"black\" /><!--" planet "-->\n")))

(defn svg-doc-header []
  (str "<?xml version=\"1.0\" standalone=\"no\"?>\n"
       "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"))

(defn write-svg [filename planets]
  (spit filename
         "<svg width=\"500\" height=\"500\" viewBox=\"0 0 500 500\" xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n"
         "<rect x=\"0\" y=\"0\" width=\"500\" height=\"500\" fill=\"none\" stroke=\"black\" stroke-width=\"1\" />\n")
  (doseq [planet (map planet-line planets)]
    (spit filename planet :append true))
  (spit filename "</svg>\n" :append true))

(defn -main
  [& args]
  (let [dt 1]
    (loop [i 1 planets [{:mass 100 :x 0 :y 0 :z 0 :vx 0 :vy 0 :vz 0} {:mass 1 :x 100000 :y 100000 :z 0 :vx 0 :vy 10 :vz 0}]]
      (write-svg "test.svg" planets)
      (println i)
      (if (< i 2)
        (recur (inc i)
               (sequence (comp (compute-velocity-t dt)
                               (compute-position-t dt))
                         (compute-acceleration planets dt)))
        i))))
