;; [[file:../../org/GridFire.org::*Spotting Model Forumulas][Spotting Model Forumulas:1]]
(ns gridfire.spotting
  (:require [clojure.core.matrix :as m]
            [gridfire.common :refer [extract-constants
                                     distance-3d
                                     fuel-moisture
                                     in-bounds?
                                     burnable?]]
            [gridfire.crown-fire :refer [Btu-ft-s->kW-m]]
            [gridfire.utils.random :refer [random-float]]
            [gridfire.conversion :as convert]
            [kixi.stats.distribution :as distribution]))

;;-----------------------------------------------------------------------------
;; Formulas
;;-----------------------------------------------------------------------------

(defn froude-number
  "Returns froude number given:
  wind-speed-20ft: (ms^-1)
  fire-line-intensity: (kWm^-1)
  temperature: (Kelvin)
  ambient-gas-density: (kgm^-3)
  specific-heat-gas: (KJkg^-1 K^-1)"
  [wind-speed-20ft fire-line-intensity temperature ambient-gas-density specific-heat-gas]
  (let [g   9.81 ;(ms^-1) gravity
        L_c (-> (/ fire-line-intensity ;characteristic length of plume
                   (* ambient-gas-density
                      specific-heat-gas
                      (convert/F->K temperature)
                      (Math/sqrt g)))
                (Math/pow (/ 2 3)))]
    (/ wind-speed-20ft
       (Math/sqrt (* g L_c)))))

(defn buoyancy-driven? [froude]
  (<= froude 1))

(defn deviation-fb
  "Returns standard deviation as described in Perryman 2013 EQ5 and EQ6 given:
  froude number: (Int)
  fire-line-intensity: (kWm^-1)
  wind-speed-20ft: (ms^-1)"
  [froude fire-line-intensity wind-speed-20ft]
  (if (buoyancy-driven? froude)
    (+ (* 0.86 (Math/pow fire-line-intensity -0.21) (Math/pow wind-speed-20ft 0.44)) 0.19)
    (- (* 4.95 (Math/pow fire-line-intensity -0.01) (Math/pow wind-speed-20ft -0.02)) 3.48)))

(defn mean-fb
  "Returns mean as described in Perryman 2013 EQ5 and EQ6 given:
  froude number: (Int)
  fire-line-intensity: (kWm^-1)
  wind-speed-20ft: (ms^-1)"
  [froude fire-line-intensity wind-speed-20ft]
  (if (buoyancy-driven? froude)
    (+ (* 1.47 (Math/pow fire-line-intensity 0.54) (Math/pow wind-speed-20ft -0.55)) 1.14)
    (- (* 1.32 (Math/pow fire-line-intensity 0.26) (Math/pow wind-speed-20ft 0.11)) 0.02)))

(defn specific-heat-dry-fuel
  "Returns specific heat of dry fuel given:
  initiial-temp: (Celcius)
  ignition-temp: (Celcius)
  "
  [initial-temp ignition-temp]
  (+ 0.266 (* 0.0016 (/ (+ ignition-temp initial-temp) 2))))

(defn heat-of-preignition
  "Returns heat of preignition given:
  init-temperature: (Celcius)
  ignition-temperature: (Celcius)
  moisture content: (Percent)"
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

(defn schroeder-ign-prob
  "Returns the probability of ignition as described in Shroeder (1969) given:
  relative-humidity: (%)
  temperature: (Farenheit)"
  [relative-humidity temperature]
  (let [ignition-temperature 320 ;;FIXME should this be a constant?
        moisture             (-> (fuel-moisture relative-humidity temperature)
                                 :dead
                                 :1hr)
        Q_ig                 (heat-of-preignition (convert/F->C temperature) ignition-temperature moisture)
        X                    (/ (- 400 Q_ig) 10)]
    (/ (* 0.000048 (Math/pow X 4.3)) 50)))

(defn schroeder-ign-prob-adjusted
  [{:keys [cell-size landfire-layers]}
   {:keys [decay-constant] :as spot-config}
   temperature
   relative-humidity
   firebrand-count
   torched-origin
   [i j :as here]]
  (let [ignition-probability (schroeder-ign-prob relative-humidity
                                                 temperature)
        distance             (distance-3d (:elevation landfire-layers)
                                          cell-size
                                          here
                                          torched-origin)
        decay-factor         (Math/exp (* -1 decay-constant distance))]
    (- 1 (Math/pow (- 1 (* ignition-probability decay-factor)) firebrand-count))))

(defn spot-ignition?
  [rand-gen spot-ignition-probability]
  (let [random-number (random-float 0 1 rand-gen)]
    (> spot-ignition-probability random-number)))

(defn spot-ignition-time [global-clock]
  global-clock)

;;-----------------------------------------------------------------------------
;; Main
;;-----------------------------------------------------------------------------

(defn sample-wind-dir-deltas
  "Returns a sequence of [x y] distances (meters) that firebrands land away
  from a torched cell at i j where:
  x: parallel to the wind
  y: perpendicular to the wind (positive values are to the right of wind direction)"
  [{:keys [fire-line-intensity] :as constants}
   {:keys [num-firebrands ambient-gas-density specific-heat-gas]}
   fire-line-intensity-matrix
   wind-speed-20ft
   temperature
   [i j]]
  (let [intensity     (convert/Btu-ft-s->kW-m (m/mget fire-line-intensity-matrix i j))
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
          (partial mapv convert/m->ft)
          vector)
         (distribution/sample num-firebrands parallel)
         (distribution/sample num-firebrands perpendicular))))

(defn hypotenuse [x y]
  (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))

(defn deltas-wind->coord
  "Converts deltas from the torched tree in the wind direction to deltas
  in the coordinate plane"
  [deltas wind-direction]
  (map (fn [[d-paral d-perp]]
         (let [H  (hypotenuse d-paral d-perp)
               t1 wind-direction
               t2 (convert/rad->deg (Math/atan (/ d-perp d-paral)))
               t3 (+ t1 t2)]
           [(* -1 H (Math/cos (convert/deg->rad t3)))
            (* H (Math/sin (convert/deg->rad t3)))]))
       deltas))

(defn firebrands
  "Returns a sequence of cells that firebrands land in"
  [[x y :as deltas] wind-direction cell cell-size]
  (let [step         (/ cell-size 2)
        cell-center  (mapv #(+ step (* % step)) cell)
        coord-deltas (deltas-wind->coord deltas wind-direction)]
    (map (comp
          (partial map int)
          (partial map #(quot % step))
          (partial map + cell-center))
         coord-deltas)))

(defn update-firebrand-counts!
  [{:keys [num-rows num-cols landfire-layers]}
   firebrand-count-matrix
   fire-spread-matrix
   [i j :as source]
   firebrands]
  (doseq [[x y :as here] firebrands
          :when          (and
                          (in-bounds? num-rows num-cols [x y])
                          (burnable? fire-spread-matrix
                                     (:fuel-model landfire-layers)
                                     source
                                     here))
          :let           [new-count (inc (m/mget firebrand-count-matrix x y))]]
    (m/mset! firebrand-count-matrix x y new-count)))

(defn spread-firebrands
  "Returns a sequence of key value pairs where
  key: [x y] locations of the cell
  val: [t p] where t = time of ignition and p = ignition-probability"
  [{:keys
    [num-rows num-cols cell-size landfire-layers wind-speed-20ft
     wind-from-direction temperature relative-humidity
     global-clock multiplier-lookup perturbations] :as constants}
   {:keys [spotting rand-gen] :as config}
   {:keys [cell fire-line-intensity crown-fire?] :as ignition-event}
   firebrand-count-matrix
   fire-spread-matrix
   fire-line-intensity-matrix]
  (when crown-fire?
    (let [{:keys
           [wind-speed-20ft
            temperature
            wind-from-direction
            relative-humidity]} (extract-constants constants global-clock cell)
          deltas                (sample-wind-dir-deltas constants
                                                        spotting
                                                        fire-line-intensity-matrix
                                                        (convert/mph->mps wind-speed-20ft)
                                                        (F->K temperature)
                                                        cell)
          wind-direction        (mod (+ 180 wind-from-direction) 360)
          firebrands            (firebrands deltas wind-direction cell cell-size)]
      (update-firebrand-counts! constants firebrand-count-matrix fire-spread-matrix cell firebrands)
      (->> (for [[x y] firebrands
                 :when (and
                        (in-bounds? num-rows num-cols [x y])
                        (burnable? fire-spread-matrix (:fuel-model landfire-layers) cell [x y]))
                 :let  [firebrand-count (m/mget firebrand-count-matrix x y)
                        spot-ignition-p (spot-ignition-probability constants
                                                                   spotting
                                                                   temperature
                                                                   relative-humidity
                                                                   firebrand-count
                                                                   cell
                                                                   [x y])]]
             (when (spot-ignition? rand-gen spot-ignition-p)
               [[x y] [(spot-ignition-time global-clock) spot-ignition-p]]))
           (remove nil?)))))
;; Spotting Model Forumulas:1 ends here
