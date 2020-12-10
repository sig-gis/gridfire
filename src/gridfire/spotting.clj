;; [[file:../../org/GridFire.org::*Spotting Model Forumulas][Spotting Model Forumulas:1]]
(ns gridfire.spotting
  (:require [clojure.core.matrix :as m]
            [kixi.stats.distribution :as distribution]))

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn F->K
  "convert farenheight to kelvin"
  [degrees]
  (->> (+ degrees 459.67)
       (* (/ 5 9))))

(defn deg->rad [d]
  (* d (/ Math/PI 180)))

(defn rad->deg [d]
  (* d (/ 180 Math/PI)))

;;-----------------------------------------------------------------------------
;; Formulas
;;-----------------------------------------------------------------------------

(defn froude-number
  [wind-speed-20ft
   fire-line-intensity
   temperature
   ambient-gas-density
   specific-heat-gas]
  (let [;;gravity
        g 9.81

        ;; characteristic length of plume
        L_c (-> (/ fire-line-intensity
                   (* ambient-gas-density
                      specific-heat-gas
                      (F->K temperature)
                      (Math/sqrt g)))
                (Math/pow (/ 2 3)))]
    (/ wind-speed-20ft
       (Math/sqrt (* g L_c)))))

(defn buoyancy-driven? [froude]
  (<= froude 1))

(defn deviation-fb
  [froude fire-line-intensity wind-speed-20ft]
  (if (buoyancy-driven? froude)
    (+ (* 0.86 (Math/pow fire-line-intensity -0.21) (Math/pow wind-speed-20ft 0.44)) 0.19)
    (- (* 4.95 (Math/pow fire-line-intensity -0.01) (Math/pow wind-speed-20ft -0.02)) 3.48)))

(defn mean-fb
  [froude fire-line-intensity wind-speed-20ft]
  (if (buoyancy-driven? froude)
    (+ (* 1.47 (Math/pow fire-line-intensity 0.54) (Math/pow wind-speed-20ft -0.55)) 1.14)
    (- (* 1.32 (Math/pow fire-line-intensity 0.26) (Math/pow wind-speed-20ft 0.11)) 0.02)))

(defn sardoy-probability
  [distance mean deviation]
  (* (/ 1
        (* (Math/sqrt (* 2 Math/PI)) deviation distance))
     (Math/exp
      (/ (* -1 (Math/pow (- (Math/log distance) mean) 2))
         (* 2 (Math/pow deviation 2))))))

;;-----------------------------------------------------------------------------
;; Main
;;-----------------------------------------------------------------------------

(defn sample-wind-dir-deltas
  "Returns a sequence of [x y] distances (meters) that firebrands land away
  from a torched cell at i j where:
  x: parallel to the wind
  y: perpendicular to the wind (positive values are to the right of wind direction)
  "
  [{:keys [firebrand-count fire-line-intensity spotting] :as constants} wind-speed-20ft temperature i j]
  (let [{:keys
         [ambient-gas-density
          specific-heat-gas]} spotting
        intensity             (m/mget fire-line-intensity i j)
        froude                (froude-number intensity wind-speed-20ft temperature ambient-gas-density specific-heat-gas)
        mean                  (mean-fb froude intensity wind-speed-20ft)
        deviation             (deviation-fb froude intensity wind-speed-20ft)
        parallel              (distribution/log-normal {:mu mean :sd deviation})
        perpendicular         (distribution/normal {:mu 0 :sd 0.92})]
    (map vector
         (distribution/sample firebrand-count parallel)
         (distribution/sample firebrand-count perpendicular))))

(defn hypotenuse [x y]
  (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))

(defn deltas-wind->coord
  "Converts deltas from the torched tree in the wind direction to deltas
  in the coordinate plane"
  [deltas wind-direction]
  (map (fn [[d-paral d-perp]]
         (let [H  (hypotenuse d-paral d-perp)
               t1 wind-direction
               t2 (rad->deg (Math/atan (/ d-perp d-paral)))
               t3 (+ t1 t2)]
           [(* H (Math/sin (deg->rad t3)))
            (* H (Math/cos (deg->rad t3)))]))
       deltas))

(defn firebrands
  "Returns a sequence cells that firebrands land in"
  [[x y :as deltas] wind-direction cell cell-size]
  (let [step         (/ cell-size 2)
        cell-center  (mapv #(+ step (* % step)) cell)
        coord-deltas (deltas-wind-dir->coord deltas wind-direction)]
    (map (comp
          (partial map int)
          (partial map #(quot step))
          (partial map + cell-center))
         coord-deltas)))
;; Spotting Model Forumulas:1 ends here
