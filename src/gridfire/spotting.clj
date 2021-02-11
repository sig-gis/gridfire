;; [[file:../../org/GridFire.org::sardoy-firebrand-dispersal][sardoy-firebrand-dispersal]]
(ns gridfire.spotting
  (:require [clojure.core.matrix :as m]
            [gridfire.common :refer [extract-constants
                                     distance-3d
                                     fuel-moisture
                                     in-bounds?
                                     burnable?]]
            [gridfire.crown-fire :refer [ft->m]]
            [gridfire.utils.random :refer [random-float my-rand-int-range]]
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
                      temperature
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

(defn- sample-num-firebrands
  [{:keys [num-firebrands]} rand-gen]
  (if (map? num-firebrands)
    (let [{:keys [lo hi]} num-firebrands
          l               (if (vector? lo) (my-rand-int-range rand-gen lo) lo)
          h               (if (vector? hi) (my-rand-int-range rand-gen hi) hi)]
      (my-rand-int-range rand-gen [l h]))
    num-firebrands))

(defn sample-wind-dir-deltas
  "Returns a sequence of [x y] distances (meters) that firebrands land away
  from a torched cell at i j where:
  x: parallel to the wind
  y: perpendicular to the wind (positive values are to the right of wind direction)
  "
  [{:keys [spotting rand-gen random-seed] :as config}
   fire-line-intensity-matrix
   wind-speed-20ft
   temperature
   [i j]]
  (let [{:keys
         [ambient-gas-density
          specific-heat-gas]} spotting
        num-firebrands        (sample-num-firebrands spotting rand-gen)
        intensity             (convert/Btu-ft-s->kW-m (m/mget fire-line-intensity-matrix i j))
        froude                (froude-number intensity
                                             wind-speed-20ft
                                             temperature
                                             ambient-gas-density
                                             specific-heat-gas)
        parallel              (distribution/log-normal {:mu (mean-fb froude intensity wind-speed-20ft)
                                                        :sd (deviation-fb froude intensity wind-speed-20ft)})
        perpendicular         (distribution/normal {:mu 0
                                                    :sd 0.92})]
    (map (comp
          (partial mapv convert/m->ft)
          vector)
         (distribution/sample num-firebrands parallel {:seed random-seed})
         (distribution/sample num-firebrands perpendicular {:seed random-seed}))))
;; sardoy-firebrand-dispersal ends here
;; [[file:../../org/GridFire.org::convert-deltas][convert-deltas]]
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
  [deltas wind-towards-direction cell cell-size]
  (let [step         (/ cell-size 2)
        cell-center  (mapv #(+ step (* % step)) cell)
        coord-deltas (deltas-wind->coord deltas wind-towards-direction)]
    (map (comp
          (partial map int)
          (partial map #(quot % step))
          (partial map + cell-center))
         coord-deltas)))
;; convert-deltas ends here
;; [[file:../../org/GridFire.org::firebrand-ignition-probability][firebrand-ignition-probability]]
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

(defn spot-ignition-probability
  [{:keys [cell-size landfire-rasters]}
   {:keys [decay-constant] :as spot-config}
   temperature
   relative-humidity
   firebrand-count
   torched-origin
   [i j :as here]]
  (let [ignition-probability (schroeder-ign-prob relative-humidity
                                                 temperature)
        distance             (ft->m (distance-3d (:elevation landfire-rasters)
                                                 cell-size
                                                 here
                                                 torched-origin))
        decay-factor         (Math/exp (* -1 decay-constant distance))]
    (- 1 (Math/pow (- 1 (* ignition-probability decay-factor)) firebrand-count))))
;; firebrand-ignition-probability ends here
;; [[file:../../org/GridFire.org::firebrands-time-of-ignition][firebrands-time-of-ignition]]
(defn spot-ignition?
  [rand-gen spot-ignition-probability]
  (let [random-number (random-float 0 1 rand-gen)]
    (> spot-ignition-probability random-number)))

(defn spot-ignition-time
  "Returns the time of spot ignition in minutes given:
  global-clock: (min)
  flame-length: (m)
  wind-speed-20ft: (ms^-1)"
  [global-clock flame-length wind-speed-20ft]
  (let [a              5.963
        b              (- a 1.4)
        D              0.003 ;firebrand diaemeter (m)
        z-max          (* 0.39 D (Math/pow 10 5))
        t-steady-state 20    ;min
        t-max-height   (convert/sec->min ;min
                            (+ (/ (* 2 flame-length) wind-speed-20ft)
                               1.2
                               (* (/ a 3.0)
                                  (- (Math/pow (/ (+ b (/ z-max flame-length)) a) (/ 3.0 2.0)) 1))))]
    (+ global-clock (* 2 t-max-height) 20)))
;; firebrands-time-of-ignition ends here
;; [[file:../../org/GridFire.org::spread-firebrands][spread-firebrands]]
(defn update-firebrand-counts!
  [{:keys [num-rows num-cols landfire-rasters]}
   firebrand-count-matrix
   fire-spread-matrix
   source
   firebrands]
  (doseq [[x y :as here] firebrands
          :when          (and (in-bounds? num-rows num-cols [x y])
                              (burnable? fire-spread-matrix
                                         (:fuel-model landfire-rasters)
                                         source
                                         here))
          :let           [new-count (inc (m/mget firebrand-count-matrix x y))]]
    (m/mset! firebrand-count-matrix x y new-count)))

(defn- in-range?
  [[min max] fuel-model-number]
  (<= min fuel-model-number max))

(defn surface-spot-percent
  [fuel-range-percents fuel-model-number]
  (reduce (fn [acc [fuel-range percent]]
            (if (in-range? fuel-range fuel-model-number)
              percent
              acc))
          0.0
          fuel-range-percents))

(defn surface-fire-spot-fire?
  "Expects surface-fire-spotting config to be a sequence of tuples of
  ranges [lo hi] and spottting percent. The range represents the range (inclusive)
  of fuel model numbers that the spotting percent is set to.
  [[[1 140] 0.0]
  [[141 149] 1.0]
  [[150 256] 1.0]]"
  [{:keys [spotting rand-gen]} {:keys [landfire-layers]} [i j] fire-line-intensity]
  (let [{:keys [surface-fire-spotting]} spotting]
    (when (and
           surface-fire-spotting
           (> fire-line-intensity (:critical-fire-line-intensity surface-fire-spotting)))
      (let [fuel-range-percents (:spotting-percent surface-fire-spotting)
            fuel-model-raster   (:fuel-model landfire-layers)
            fuel-model-number   (int (m/mget fuel-model-raster i j))
            spot-percent        (surface-spot-percent fuel-range-percents fuel-model-number)]
        (>= spot-percent (random-float 0.0 1.0 rand-gen))))))

(defn crown-spot-fire? [{:keys [spotting rand-gen]}]
  (when-let [spot-percent (:crown-fire-spotting-percent spotting)]
    (let [p (if (vector? spot-percent)
              (let [[lo hi] spot-percent]
                (random-float lo hi rand-gen))
              spot-percent)]
      (>= p (random-float 0.0 1.0 rand-gen)))))

(defn spot-fire? [config constants crown-fire? here fire-line-intensity]
  (if crown-fire?
    (crown-spot-fire? config)
    (surface-fire-spot-fire? config constants here fire-line-intensity)))

(defn spread-firebrands
  "Returns a sequence of key value pairs where
  key: [x y] locations of the cell
  val: [t p] where:
  t: time of ignition
  p: ignition-probability"
  [{:keys
    [num-rows num-cols cell-size landfire-rasters global-clock] :as constants}
   {:keys [spotting rand-gen] :as config}
   {:keys [firebrand-count-matrix
           fire-spread-matrix
           fire-line-intensity-matrix
           flame-length-matrix]}
   {:keys [cell fire-line-intensity crown-fire?]}]
  (when (spot-fire? config constants crown-fire? cell fire-line-intensity)
    (let [{:keys
           [wind-speed-20ft
            temperature
            wind-from-direction
            relative-humidity]} (extract-constants constants global-clock cell)
          deltas                (sample-wind-dir-deltas config
                                                        fire-line-intensity-matrix
                                                        (convert/mph->mps wind-speed-20ft)
                                                        (convert/F->K temperature)
                                                        cell)
          wind-to-direction     (mod (+ 180 wind-from-direction) 360)
          firebrands            (firebrands deltas wind-to-direction cell cell-size)]
      (update-firebrand-counts! constants firebrand-count-matrix fire-spread-matrix cell firebrands)
      (->> (for [[x y] firebrands
                 :when (and (in-bounds? num-rows num-cols [x y])
                            (burnable? fire-spread-matrix (:fuel-model landfire-rasters) cell [x y]))
                 :let  [firebrand-count (m/mget firebrand-count-matrix x y)
                        spot-ignition-p (spot-ignition-probability constants
                                                                   spotting
                                                                   temperature
                                                                   relative-humidity
                                                                   firebrand-count
                                                                   cell
                                                                   [x y])]]
             (when (spot-ignition? rand-gen spot-ignition-p)
               (let [[i j] cell
                     t     (spot-ignition-time global-clock
                                               (ft->m (m/mget flame-length-matrix i j))
                                               (convert/mph->mps wind-speed-20ft))]
                 [[x y] [t spot-ignition-p]])))
           (remove nil?)))))
;; spread-firebrands ends here
