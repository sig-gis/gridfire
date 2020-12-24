;; [[file:../../org/GridFire.org::*Spotting Model Forumulas][Spotting Model Forumulas:1]]
(ns gridfire.spotting
  (:require [clojure.core.matrix :as m]
            [gridfire.common :refer [sample-at
                                     distance-3d
                                     fuel-moisture
                                     in-bounds?]]
            [gridfire.fuel-models :refer [build-fuel-model
                                          moisturize
                                          size-class-sum
                                          category-sum]]
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

(defn m->ft [m]
  (* m 3.281))

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

(defn specific-heat-dry-fuel [initial-temp ignition-temp]
  (+ 0.266 0.0016 (/ (+ ignition-temp initial-temp) 2)))

(defn heat-of-preignition
  [init-temperature ignition-temperature moisture]
  (let [T_o init-temperature
        T_i ignition-temperature
        M   moisture
        c_f (specific-heat-dry-fuel T_o T_i)

        ;; heat required to reach ignition temperature
        Q_a (* (- T_i T_o) c_f)

        ;; heat required to raise moisture to reach boiling point
        Q_b (* (- 100 T_o) M)

        ;; Heat of desorption
        Q_c (* 18.54 (- 1 (Math/exp (* -15.1 M))))

        ;; Heat required to vaporize moisture
        Q_d (* 540 M)]
    (+ Q_a Q_b Q_c Q_d)))

(defn schroeder-ignition-probability [fuel-model-number relative-humidity temperature]
  (let [ignition-temperature 320 ;;FIXME should this be a constant?
        fuel-moisture        (fuel-moisture relative-humidity temperature)
        moisture-model       (as-> (int fuel-model-number) $
                                     (build-fuel-model $)
                                     (moisturize $ fuel-moisture)
                                     (:M_f $))
        m                    (size-class-sum (fn [i j] (-> moisture-model i j)))
        total-moisture       (category-sum (fn [i] (-> m i)))
        Q_ig                 (heat-of-preignition temperature ignition-temperature total-moisture)
        X                    (/ (- 400 Q_ig) 10)]
    (/ (* 0.000048 (Math/pow X 4.3)) 50)))

(defn spot-ignition-probability
  [{:keys
    [cell-size global-clock landfire-layers
     multiplier-lookup perturbations]}
   {:keys [decay-constant] :as spot-config}
   temperature
   relative-humidity
   firebrand-count
   torched-origin
   [i j :as here]]
  (let [fuel-model-number    (m/mget (:fuel-model landfire-layers) i j)
        ignition-probability (schroeder-ignition-probability fuel-model-number
                                                             relative-humidity
                                                             temperature)
        distance             (distance-3d (:elevation landfire-layers)
                                          cell-size
                                          here
                                          torched-origin)
        decay-factor         (Math/exp (* decay-constant distance))]
    (- 1 (Math/pow (- 1 (* ignition-probability decay-factor)) firebrand-count))))

;;-----------------------------------------------------------------------------
;; Main
;;-----------------------------------------------------------------------------

(defn sample-wind-dir-deltas
  "Returns a sequence of [x y] distances (meters) that firebrands land away
  from a torched cell at i j where:
  x: parallel to the wind
  y: perpendicular to the wind (positive values are to the right of wind direction)
  "
  [fire-line-intensity-matrix
   {:keys [num-firebrands ambient-gas-density specific-heat-gas]}
   wind-speed-20ft
   temperature
   [i j]]
  (let [intensity     (m/mget fire-line-intensity-matrix i j)
        froude        (froude-number intensity
                                     wind-speed-20ft
                                     temperature
                                     ambient-gas-density
                                     specific-heat-gas)
        mean          (mean-fb froude intensity wind-speed-20ft)
        deviation     (deviation-fb froude intensity wind-speed-20ft)
        parallel      (distribution/log-normal {:mu mean :sd deviation})
        perpendicular (distribution/normal {:mu 0 :sd 0.92})]
    (map (comp
          (partial mapv m->ft)
          vector)
         (distribution/sample num-firebrands parallel)
         (distribution/sample num-firebrands perpendicular))))

(defn hypotenuse [x y]
  (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))

(defn deltas-wind-dir->coord
  "Converts deltas from the torched tree in the wind direction to deltas
  in the coordinate plane"
  [deltas wind-direction]
  (map (fn [[d-paral d-perp]]
         (let [H  (hypotenuse d-paral d-perp)
               t1 wind-direction
               t2 (rad->deg (Math/atan (/ d-perp d-paral)))
               t3 (+ t1 t2)]
           [(* H (Math/sin (deg->rad t3)))
            (* -1 H (Math/cos (deg->rad t3)))]))
       deltas))

(defn firebrands
  "Returns a sequence cells that firebrands land in"
  [[x y :as deltas] wind-direction cell cell-size]
  (let [step         (/ cell-size 2)
        cell-center  (mapv #(+ step (* % step)) cell)
        coord-deltas (deltas-wind-dir->coord deltas wind-direction)]
    (map (comp
          (partial map int)
          (partial map #(quot % step))
          (partial map + cell-center))
         coord-deltas)))

(defn spread-firebrands
  [{:keys [num-rows num-cols cell-size wind-speed-20ft wind-from-direction temperature]}
   spot-config
   {:keys [cell crown-fire?]}
   firebrand-count-matrix
   fire-line-intensity-matrix]
  (when crown-fire?
    (let [deltas (sample-wind-dir-deltas fire-line-intensity-matrix spot-config wind-speed-20ft temperature cell)]
      (doseq [[x y] (firebrands deltas wind-from-direction cell cell-size)
              :when (in-bounds? num-rows num-cols [x y])]
        (let [count (m/mget firebrand-count-matrix x y)]
          (m/mset! firebrand-count-matrix x y (inc count)))))))
;; Spotting Model Forumulas:1 ends here
