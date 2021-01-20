;; [[file:../../org/GridFire.org::fire-spread-algorithm][fire-spread-algorithm]]
(ns gridfire.fire-spread
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as mop]
            [gridfire.common :refer [burnable-fuel-model?
                                     burnable?
                                     extract-constants
                                     fuel-moisture in-bounds?]]
            [gridfire.crown-fire :refer [crown-fire-eccentricity
                                         crown-fire-line-intensity
                                         cruz-crown-fire-spread van-wagner-crown-fire-initiation?]]
            [gridfire.fuel-models :refer [build-fuel-model
                                          moisturize]]
            [gridfire.perturbation :as perturbation]
            [gridfire.spotting :as spot]
            [gridfire.surface-fire :refer [anderson-flame-depth
                                           byram-fire-line-intensity
                                           byram-flame-length
                                           rothermel-surface-fire-spread-any
                                           rothermel-surface-fire-spread-max
                                           rothermel-surface-fire-spread-no-wind-no-slope
                                           wind-adjustment-factor]]))

(m/set-current-implementation :vectorz)

;; for surface fire, tau = 10 mins, t0 = 0, and t = global-clock
;; for crown fire, tau = 20 mins, t0 = time of first torch, t = global-clock
;; (defn lautenberger-spread-acceleration
;;   [equilibrium-spread-rate t0 t tau]
;;   (* equilibrium-spread-rate (- 1.0 (Math/exp (/ (- t0 t 0.2) tau)))))
;;
;; Note: Because of our use of adaptive timesteps, if the spread rate on
;;       the first timestep is not at least 83 ft/min, then the timestep will
;;       be calculated as greater than 60 minutes, which will terminate the
;;       one hour fire simulation instantly.

(defn random-cell
  "Returns a random [i j] pair with i < num-rows and j < num-cols."
  [num-rows num-cols]
  [(rand-int num-rows)
   (rand-int num-cols)])

(defn get-neighbors
  "Returns the eight points adjacent to the passed-in point."
  [[i j]]
  (let [i- (- i 1)
        i+ (+ i 1)
        j- (- j 1)
        j+ (+ j 1)]
    (vector [i- j-] [i- j] [i- j+]
            [i  j-]        [i  j+]
            [i+ j-] [i+ j] [i+ j+])))

(defn distance-3d
  "Returns the terrain distance between two points in feet."
  [elevation-matrix cell-size [i1 j1] [i2 j2]]
  (let [di (* cell-size (- i1 i2))
        dj (* cell-size (- j1 j2))
        dz (- (m/mget elevation-matrix i1 j1)
              (m/mget elevation-matrix i2 j2))]
    (Math/sqrt (+ (* di di) (* dj dj) (* dz dz)))))

(def offset-to-degrees
  "Returns clockwise degrees from north."
  {[-1  0]   0.0   ; N
   [-1  1]  45.0   ; NE
   [ 0  1]  90.0   ; E
   [ 1  1] 135.0   ; SE
   [ 1  0] 180.0   ; S
   [ 1 -1] 225.0   ; SW
   [ 0 -1] 270.0   ; W
   [-1 -1] 315.0}) ; NW

(defn rothermel-fast-wrapper [fuel-model-number fuel-moisture]
  (let [fuel-model      (-> (build-fuel-model (int fuel-model-number))
                            (moisturize fuel-moisture))
        spread-info-min (rothermel-surface-fire-spread-no-wind-no-slope fuel-model)]
    [fuel-model spread-info-min]))
(def rothermel-fast-wrapper (memoize rothermel-fast-wrapper))

(defn compute-burn-trajectory
  [neighbor here spread-info-min spread-info-max fuel-model crown-bulk-density
   canopy-cover canopy-height canopy-base-height foliar-moisture crown-spread-max
   crown-eccentricity landfire-layers cell-size overflow-trajectory overflow-heat]
  (let [trajectory          (mop/- neighbor here)
        spread-direction    (offset-to-degrees trajectory)
        surface-spread-rate (rothermel-surface-fire-spread-any spread-info-max
                                                               spread-direction)
        residence-time      (:residence-time spread-info-min)
        reaction-intensity  (:reaction-intensity spread-info-min)
        surface-intensity   (->> (anderson-flame-depth surface-spread-rate residence-time)
                                 (byram-fire-line-intensity reaction-intensity))
        crown-fire?         (van-wagner-crown-fire-initiation? canopy-cover
                                                               canopy-base-height
                                                               foliar-moisture
                                                               surface-intensity)
        crown-spread-rate   (if crown-fire?
                              (rothermel-surface-fire-spread-any
                               (assoc spread-info-max
                                      :max-spread-rate crown-spread-max
                                      :eccentricity crown-eccentricity)
                               spread-direction))
        crown-intensity     (if crown-fire?
                              (crown-fire-line-intensity
                               crown-spread-rate
                               crown-bulk-density
                               canopy-height
                               canopy-base-height
                               (-> fuel-model :h :dead :1hr)))
        spread-rate         (if crown-fire?
                              (max surface-spread-rate crown-spread-rate)
                              surface-spread-rate)
        fire-line-intensity (if crown-fire?
                              (+ surface-intensity crown-intensity)
                              surface-intensity)
        flame-length        (byram-flame-length fire-line-intensity)]
    {:cell                neighbor
     :trajectory          trajectory
     :terrain-distance    (distance-3d (:elevation landfire-layers) cell-size here neighbor)
     :spread-rate         spread-rate
     :fire-line-intensity fire-line-intensity
     :flame-length        flame-length
     :fractional-distance (volatile! (if (= trajectory overflow-trajectory)
                                       overflow-heat
                                       0.0))
     :crown-fire?         crown-fire?}))

(defn compute-neighborhood-fire-spread-rates!
  "Returns a vector of entries of the form {:cell [i j], :trajectory [di dj],
  :terrain-distance ft, :spread-rate ft/min, :fire-line-intensity Btu/ft/s, :flame-length ft,
  :fractional-distance [0-1]}, one for each cell adjacent to here."
  [{:keys [landfire-layers foliar-moisture ellipse-adjustment-factor cell-size num-rows num-cols] :as constants}
   fire-spread-matrix
   [i j :as here]
   overflow-trajectory
   overflow-heat
   global-clock]
  (let [{:keys
         [aspect
          canopy-base-height
          canopy-cover
          canopy-height
          crown-bulk-density
          fuel-model
          relative-humidity
          slope
          temperature
          wind-from-direction
          wind-speed-20ft]}          (extract-constants constants global-clock here)
        fuel-moisture                (fuel-moisture relative-humidity temperature)
        [fuel-model spread-info-min] (rothermel-fast-wrapper fuel-model fuel-moisture)
        midflame-wind-speed          (* wind-speed-20ft 88.0
                                        (wind-adjustment-factor (:delta fuel-model) canopy-height canopy-cover)) ; mi/hr -> ft/min
        spread-info-max              (rothermel-surface-fire-spread-max spread-info-min
                                                                        midflame-wind-speed
                                                                        wind-from-direction
                                                                        slope
                                                                        aspect
                                                                        ellipse-adjustment-factor)
        crown-spread-max             (cruz-crown-fire-spread wind-speed-20ft crown-bulk-density
                                                             (-> fuel-moisture :dead :1hr))
        crown-eccentricity           (crown-fire-eccentricity wind-speed-20ft
                                                              ellipse-adjustment-factor)
        ignition-probability         (m/mget fire-spread-matrix i j)]
    (into []
          (comp
           (filter #(and (in-bounds? num-rows num-cols %)
                         (burnable? fire-spread-matrix (:fuel-model landfire-layers) here %)))
           (map #(compute-burn-trajectory % here spread-info-min spread-info-max fuel-model
                                          crown-bulk-density canopy-cover canopy-height
                                          canopy-base-height foliar-moisture crown-spread-max
                                          crown-eccentricity landfire-layers cell-size
                                          overflow-trajectory overflow-heat)))
          (get-neighbors here))))

(defn burnable-neighbors?
  [fire-spread-matrix fuel-model-matrix num-rows num-cols cell]
  (some #(and (in-bounds? num-rows num-cols %)
              (burnable? fire-spread-matrix fuel-model-matrix cell %))
        (get-neighbors cell)))

(defn select-random-ignition-site
  [fuel-model-matrix]
  (let [num-rows           (m/row-count    fuel-model-matrix)
        num-cols           (m/column-count fuel-model-matrix)
        fire-spread-matrix (m/zero-matrix num-rows num-cols)]
    (loop [[i j :as ignition-site] (random-cell num-rows num-cols)]
      (if (and (burnable-fuel-model? (m/mget fuel-model-matrix i j))
               (burnable-neighbors? fire-spread-matrix fuel-model-matrix
                                    num-rows num-cols ignition-site))
        ignition-site
        (recur (random-cell num-rows num-cols))))))

(defn identify-ignition-events
  [ignited-cells timestep fire-spread-matrix]
  (->> (for [[source destinations] ignited-cells
             {:keys [cell
                     trajectory
                     terrain-distance
                     spread-rate
                     flame-length
                     fire-line-intensity
                     fractional-distance
                     crown-fire?]} destinations]
         (let [[i j]               source
               new-spread-fraction (/ (* spread-rate timestep) terrain-distance)
               new-total           (vreset! fractional-distance
                                            (+ @fractional-distance new-spread-fraction))]
           (if (>= new-total 1.0)
             {:cell                 cell
              :trajectory           trajectory
              :fractional-distance  @fractional-distance
              :flame-length         flame-length
              :fire-line-intensity  fire-line-intensity
              :crown-fire?          crown-fire?
              :ignition-probability (m/mget fire-spread-matrix i j)})))
       (remove nil?)
       (group-by :cell)
       (map (fn [[cell trajectories]] (apply max-key :fractional-distance trajectories)))
       (into [])))

(defn update-ignited-cells
  [{:keys [landfire-layers num-rows num-cols] :as constants}
   ignited-cells
   ignition-events
   fire-spread-matrix
   global-clock]
  (let [newly-ignited-cells (into #{} (map :cell) ignition-events)
        fuel-model-matrix   (:fuel-model landfire-layers)]
    (into {}
          (concat
           (for [[cell spread-info] ignited-cells
                 :when              (burnable-neighbors? fire-spread-matrix
                                                         fuel-model-matrix
                                                         num-rows
                                                         num-cols
                                                         cell)]
             [cell (remove #(contains? newly-ignited-cells (:cell %)) spread-info)])
           (for [{:keys
                  [cell
                   trajectory
                   fractional-distance]} ignition-events
                 :when                   (burnable-neighbors? fire-spread-matrix
                                                              fuel-model-matrix
                                                              num-rows num-cols
                                                              cell)]
             [cell (compute-neighborhood-fire-spread-rates!
                    constants
                    fire-spread-matrix
                    cell
                    trajectory
                    (- fractional-distance 1.0)
                    global-clock)])))))

(defn generate-ignited-cells
  [constants fire-spread-matrix cells]
  (when (seq cells)
    (into {}
          (for [cell cells]
            [cell (compute-neighborhood-fire-spread-rates!
                   constants
                   fire-spread-matrix
                   cell
                   nil
                   0.0
                   0.0)]))))

(defn identify-spot-ignition-events
  [global-clock spot-ignitions]
  (let [to-ignite-now (group-by (fn [[cell [time ign-prob]]]
                                  (>= global-clock time))
                                spot-ignitions)
        ignite-later  (into {} (get to-ignite-now false))
        ignite-now    (into {} (get to-ignite-now true))]
    [ignite-later ignite-now]))

(defn spot-ignited-cells
  "Updates matrices for spot ignited cells
  Returns a map of ignited cells"
  [constants
   global-clock
   {:keys [fire-spread-matrix burn-time-matrix]}
   spot-ignite-now]
  (let [ignited?        (fn [[k v]]
                          (let [[i j] k
                                [_ p] v]
                            (> (m/mget fire-spread-matrix i j) p)))
        spot-ignite-now (remove ignited? spot-ignite-now)
        ignited-cells   (generate-ignited-cells constants
                                                fire-spread-matrix
                                                (keys spot-ignite-now))]
    (doseq [cell spot-ignite-now
            :let [[i j]                    (key cell)
                  [_ ignition-probability] (val cell)]]
      (m/mset! fire-spread-matrix i j ignition-probability)
      (m/mset! burn-time-matrix i j global-clock))
    ignited-cells))

(defn new-spot-ignitions
  "Returns a map of [x y] locations to [t p] where:
  t: time of ignition
  p: ignition-probability"
  [{:keys [spotting] :as config} constants matrices ignition-events]
  (when spotting
    (reduce (fn [acc ignition-event]
              (merge-with (partial min-key first)
                          acc
                          (->> (spot/spread-firebrands
                                constants
                                config
                                matrices
                                ignition-event)
                               (into {}))))
            {}
            ignition-events)))


(defn run-loop
  [{:keys [max-runtime cell-size initial-ignition-site multiplier-lookup] :as constants}
   {:keys [spotting] :as config}
   {:keys [fire-spread-matrix
           flame-length-matrix
           fire-line-intensity-matrix
           firebrand-count-matrix
           burn-time-matrix] :as matrices}
   ignited-cells]
  (loop [global-clock      0.0
         ignited-cells     ignited-cells
         spot-ignitions {}]
    (if (and (< global-clock max-runtime)
             (seq ignited-cells))
      (let [dt                (->> ignited-cells
                                   (vals)
                                   (apply concat)
                                   (map :spread-rate)
                                   (reduce max 0.0)
                                   (/ cell-size))
            timestep          (if (> (+ global-clock dt) max-runtime)
                                (- max-runtime global-clock)
                                dt)
            next-global-clock (+ global-clock timestep)
            ignition-events   (identify-ignition-events ignited-cells timestep fire-spread-matrix)
            constants         (perturbation/update-global-vals constants global-clock next-global-clock)]
        ;; [{:cell :trajectory :fractional-distance
        ;;   :flame-length :fire-line-intensity} ...]
        (doseq [{:keys [cell flame-length fire-line-intensity
                        ignition-probability] :as ignition-event} ignition-events]
          (let [[i j] cell]
            (m/mset! fire-spread-matrix         i j ignition-probability)
            (m/mset! flame-length-matrix        i j flame-length)
            (m/mset! fire-line-intensity-matrix i j fire-line-intensity)
            (m/mset! burn-time-matrix           i j global-clock)))
        (let [new-spot-ignitions (new-spot-ignitions config
                                                     (assoc constants :global-clock global-clock)
                                                     matrices
                                                     ignition-events)
              [spot-ignite-later
               spot-ignite-now]  (identify-spot-ignition-events global-clock
                                                                (merge-with (partial min-key first)
                                                                            spot-ignitions
                                                                            new-spot-ignitions))
              spot-ignited-cells (spot-ignited-cells constants
                                                     global-clock
                                                     matrices
                                                     spot-ignite-now)]
          (recur next-global-clock
                 (update-ignited-cells constants
                                       (merge spot-ignited-cells ignited-cells)
                                       ignition-events
                                       fire-spread-matrix
                                       global-clock)
                 spot-ignite-later)))
      {:global-clock               global-clock
       :initial-ignition-site      initial-ignition-site
       :ignited-cells              (keys ignited-cells)
       :fire-spread-matrix         fire-spread-matrix
       :flame-length-matrix        flame-length-matrix
       :fire-line-intensity-matrix fire-line-intensity-matrix
       :burn-time-matrix           burn-time-matrix})))

(defmulti run-fire-spread
  "Runs the raster-based fire spread model with a map of these arguments:
  - max-runtime: double (minutes)
  - cell-size: double (feet)
  - landfire-layers: Consisting of these matrices
    - elevation: core.matrix 2D double array (feet)
    - slope: core.matrix 2D double array (vertical feet/horizontal feet)
    - aspect: core.matrix 2D double array (degrees clockwise from north)
    - fuel-model: core.matrix 2D double array (fuel model numbers 1-256)
    - canopy-height: core.matrix 2D double array (feet)
    - canopy-base-height: core.matrix 2D double array (feet)
    - crown-bulk-density: core.matrix 2D double array (lb/ft^3)
    - canopy-cover: core.matrix 2D double array (0-100)
  - wind-speed-20ft: double (miles/hour)
  - wind-from-direction: double (degrees clockwise from north)
  - fuel-moisture: doubles (%){:dead {:1hr :10hr :100hr} :live {:herbaceous :woody}}
  - foliar-moisture: double (%)
  - ellipse-adjustment-factor: (< 1.0 = more circular, > 1.0 = more elliptical)
  - initial-ignition-site: One of the following:
     - point represented as [row col]
     - map of ignition matrices (firespread, flame length, and fireline intensity)
     - (randomly chosen point if omitted)
  - num-rows: integer
  - num-cols: integer"
  (fn
    ([constants config] :add-default)

    ([constants config initial-ignition-site]
     (type initial-ignition-site))))

(defmethod run-fire-spread :add-default
  [{:keys [landfire-layers] :as constants}]
  (let [ignition-site (select-random-ignition-site (:fuel-model landfire-layers))]
    (run-fire-spread constants ignition-site)))

(defmethod run-fire-spread clojure.lang.PersistentVector
  [{:keys [landfire-layers num-rows num-cols spotting] :as constants}
   {:keys [spotting] :as config}
   [i j :as initial-ignition-site]]
  (let [fuel-model-matrix          (:fuel-model landfire-layers)
        fire-spread-matrix         (m/zero-matrix num-rows num-cols)
        flame-length-matrix        (m/zero-matrix num-rows num-cols)
        fire-line-intensity-matrix (m/zero-matrix num-rows num-cols)
        burn-time-matrix           (m/zero-matrix num-rows num-cols)
        firebrand-count-matrix     (when spotting (m/zero-matrix num-rows num-cols))]
    (when (and (in-bounds? num-rows num-cols initial-ignition-site)
               (burnable-fuel-model? (m/mget fuel-model-matrix i j))
               (burnable-neighbors? fire-spread-matrix fuel-model-matrix
                                    num-rows num-cols initial-ignition-site))
      ;; initialize the ignition site
      (m/mset! fire-spread-matrix i j 1.0)
      (m/mset! flame-length-matrix i j 1.0)
      (m/mset! fire-line-intensity-matrix i j 1.0)
      (let [ignited-cells {initial-ignition-site
                           (compute-neighborhood-fire-spread-rates!
                            constants
                            fire-spread-matrix
                            initial-ignition-site
                            nil
                            0.0
                            0.0)}]
        (run-loop (merge
                   constants
                   {:initial-ignition-site initial-ignition-site})
                  config
                  {:fire-spread-matrix         fire-spread-matrix
                   :flame-length-matrix        flame-length-matrix
                   :fire-line-intensity-matrix fire-line-intensity-matrix
                   :firebrand-count-matrix     firebrand-count-matrix
                   :burn-time-matrix           burn-time-matrix}
                  ignited-cells)))))

(defn- initialize-matrix
  [num-rows num-cols indices]
  (let [matrix (m/zero-matrix num-rows num-cols)]
    (doseq [[i j] indices
            :when (in-bounds? num-rows num-cols [i j])]
      (m/mset! matrix i j -1))
    matrix))

(defn- get-non-zero-indices [m]
  (for [[r cols] (map-indexed vector (m/non-zero-indices m))
        c        cols]
    [r c]))

(defmethod run-fire-spread clojure.lang.PersistentHashMap
  [{:keys [landfire-layers num-rows num-cols] :as constants}
   {:keys [spotting] :as config}
   initial-ignition-raster]
  (let [fire-spread-matrix         (m/mutable (:matrix initial-ignition-raster))
        non-zero-indices           (get-non-zero-indices fire-spread-matrix)
        flame-length-matrix        (initialize-matrix num-rows num-cols non-zero-indices)
        fire-line-intensity-matrix (initialize-matrix num-rows num-cols non-zero-indices)
        burn-time-matrix           (initialize-matrix num-rows num-cols non-zero-indices)
        firebrand-count-matrix     (when spotting (m/zero-matrix num-rows num-cols))
        ignited-cells              (generate-ignited-cells constants fire-spread-matrix non-zero-indices)]
    (run-loop (merge
               constants
               {:initial-ignition-site initial-ignition-raster})
              config
              {:fire-spread-matrix         fire-spread-matrix
               :flame-length-matrix        flame-length-matrix
               :fire-line-intensity-matrix fire-line-intensity-matrix
               :firebrand-count-matrix     firebrand-count-matrix
               :burn-time-matrix           burn-time-matrix}
              ignited-cells)))
;; fire-spread-algorithm ends here
