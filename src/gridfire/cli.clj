;; [[file:../../org/GridFire.org::command-line-interface][command-line-interface]]
(ns gridfire.cli
  (:gen-class)
  (:require [clojure.core.matrix :as m]
            [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [gridfire.crown-fire :refer [m->ft]]
            [gridfire.fetch :as fetch]
            [gridfire.fire-spread :refer [run-fire-spread]]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix]]
            [gridfire.postgis-bridge :refer [postgis-raster-to-matrix]]
            [gridfire.surface-fire :refer [degrees-to-radians]]
            [gridfire.spec.config :as spec]
            [gridfire.utils.random :refer [my-rand-int my-rand-nth random-float]]
            [gridfire.perturbation :as perturbation]
            [magellan.core :refer [make-envelope
                                   matrix-to-raster
                                   register-new-crs-definitions-from-properties-file!
                                   write-raster]]
            [matrix-viz.core :refer [save-matrix-as-png]])
  (:import java.util.Random))

(m/set-current-implementation :vectorz)

(register-new-crs-definitions-from-properties-file! "CUSTOM"
                                                    (io/resource "custom_projections.properties"))

(def layer-names
  [:aspect
   :canopy-base-height
   :canopy-cover
   :canopy-height
   :crown-bulk-density
   :elevation
   :fuel-model
   :slope])

(defn convert-metrics
  "Converting metrics in layers:
  meters to feet
  degrees to percent"
  [landfire-layers]
  (-> landfire-layers
      (update-in [:elevation :matrix]
                 (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
      (update-in [:slope :matrix]
                 (fn [matrix] (m/emap #(Math/tan (degrees-to-radians %)) matrix))) ; degrees -> %
      (update-in [:canopy-height :matrix]
                 (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
      (update-in [:canopy-base-height :matrix]
                 (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
      (update-in [:crown-bulk-density :matrix]
                 (fn [matrix] (m/emap #(* % 0.0624) matrix))))) ; kg/m^3 -> lb/ft^3

(defmulti fetch-landfire-layers
  "Returns a map of LANDFIRE rasters (represented as maps) with the following units:
   {:elevation          feet
    :slope              vertical feet/horizontal feet
    :aspect             degrees clockwise from north
    :fuel-model         fuel model numbers 1-256
    :canopy-height      feet
    :canopy-base-height feet
    :crown-bulk-density lb/ft^3
    :canopy-cover       % (0-100)}"
  (fn [config]
    (:fetch-layer-method config)))

(defmethod fetch-landfire-layers :postgis
  [{:keys [db-spec landfire-layers]}]
  (convert-metrics
   (reduce (fn [amap layer-name]
             (let [table (landfire-layers layer-name)]
               (assoc amap layer-name
                      (postgis-raster-to-matrix db-spec table))))
           {}
           layer-names)))

(defmethod fetch-landfire-layers :geotiff
  [{:keys [landfire-layers]}]
  (convert-metrics
   (reduce (fn [amap layer-name]
             (let [file-name (landfire-layers layer-name)]
               (assoc amap layer-name
                      (geotiff-raster-to-matrix file-name))))
           {}
           layer-names)))

(defn sample-from-list
  [rand-generator n xs]
  (repeatedly n #(my-rand-nth rand-generator xs)))

(defn sample-from-range
  [rand-generator n [min max]]
  (let [range (- max min)]
    (repeatedly n #(+ min (my-rand-int rand-generator range)))))

(defn draw-samples
  [rand-generator n x]
  (into []
        (cond (list? x)   (sample-from-list rand-generator n x)
              (vector? x) (sample-from-range rand-generator n x)
              :else       (repeat n x))))

(defn cells-to-acres
  [cell-size num-cells]
  (let [acres-per-cell (/ (* cell-size cell-size) 43560.0)]
    (* acres-per-cell num-cells)))

(defn summarize-fire-spread-results
  [fire-spread-results cell-size]
  (let [flame-lengths              (filterv pos? (m/eseq (:flame-length-matrix fire-spread-results)))
        fire-line-intensities      (filterv pos? (m/eseq (:fire-line-intensity-matrix fire-spread-results)))
        burned-cells               (count flame-lengths)
        fire-size                  (cells-to-acres cell-size burned-cells)
        flame-length-mean          (/ (m/esum flame-lengths) burned-cells)
        fire-line-intensity-mean   (/ (m/esum fire-line-intensities) burned-cells)
        flame-length-stddev        (->> flame-lengths
                                        (m/emap #(Math/pow (- flame-length-mean %) 2.0))
                                        (m/esum)
                                        (#(/ % burned-cells))
                                        (Math/sqrt))
        fire-line-intensity-stddev (->> fire-line-intensities
                                        (m/emap #(Math/pow (- fire-line-intensity-mean %) 2.0))
                                        (m/esum)
                                        (#(/ % burned-cells))
                                        (Math/sqrt))]
    {:fire-size                  fire-size
     :flame-length-mean          flame-length-mean
     :flame-length-stddev        flame-length-stddev
     :fire-line-intensity-mean   fire-line-intensity-mean
     :fire-line-intensity-stddev fire-line-intensity-stddev}))

(defn calc-emc
  "Computes the Equilibrium Moisture Content (EMC) from rh (relative
   humidity in %) and temp (temperature in F)."
  [rh temp]
  (/ (cond (< rh 10)  (+ 0.03229 (* 0.281073 rh) (* -0.000578 rh temp))
           (< rh 50)  (+ 2.22749 (* 0.160107 rh) (* -0.01478 temp))
           :otherwise (+ 21.0606 (* 0.005565 rh rh) (* -0.00035 rh temp) (* -0.483199 rh)))
     30))

(defn calc-ffwi
  "Computes the Fosberg Fire Weather Index value from rh (relative
   humidity in %), temp (temperature in F), wsp (wind speed in mph),
   and a constant x (gust multiplier).
   ------------------------------------------------------------------
   Note: ffwi can be computed with (calc-ffwi rh temp wsp 1.0)
         ffwi-max can be computed with (calc-ffwi minrh maxtemp wsp 1.75)
   Geek points: Uses Cramer's rule: (+ d (* x (+ c (* x (+ b (* x a))))))
                for an efficient cubic calculation on tmp."
  [rh temp wsp x]
  (let [m   (calc-emc rh temp)
        eta (+ 1 (* m (+ -2 (* m (+ 1.5 (* m -0.5))))))]
    (/ (* eta (Math/sqrt (+ 1 (Math/pow (* x wsp) 2))))
       0.3002)))

(defn kebab->snake [s]
  (str/replace s #"-" "_"))

(defn layer-matrix [fire-spread-results layer]
  (let [kw (->> (str (name layer) "-matrix")
                keyword)]
   (get fire-spread-results kw)))

(defn process-output-layers-timestep!
  [{:keys [output-layers output-geotiffs? output-pngs? output-dir]}
   {:keys [global-clock burn-time-matrix] :as fire-spread-results}
   envelope
   simulation-id]
  (doseq [[layer timestep] output-layers
          output-time      (range 0 (inc global-clock) timestep)]
    (let [filtered-matrix (m/emap (fn [layer-value burn-time]
                                    (if (<= burn-time output-time)
                                      layer-value
                                      0.0))
                                  (layer-matrix fire-spread-results layer)
                                  burn-time-matrix)
          layer-name      (-> (name layer)
                              kebab->snake)]
      (do
        (when output-geotiffs?
          (-> (matrix-to-raster layer-name filtered-matrix envelope)
              (write-raster (str/join "_" [layer-name
                                           simulation-id
                                           (str "t" output-time ".tif")]))))
        (when output-pngs?
          (save-matrix-as-png :color 4 -1.0
                              filtered-matrix
                              (str/join "_" [layer-name
                                             simulation-id
                                             (str "t" output-time ".png")])))))))

(defn process-output-layers!
  [{:keys [output-geotiffs? outfile-suffix output-pngs?] :as config}
   fire-spread-results
   envelope
   simulation-id]
  (doseq [[name layer] [["fire_spread"         :fire-spread-matrix]
                        ["flame_length"        :flame-length-matrix]
                        ["fire_line_intensity" :fire-line-intensity-matrix]]]
    (do
      (when output-geotiffs?
        (-> (matrix-to-raster name (fire-spread-results layer) envelope)
            (write-raster (str name outfile-suffix "_" simulation-id ".tif"))))
      (when output-pngs?
        (save-matrix-as-png :color 4 -1.0
                            (fire-spread-results layer)
                            (str name outfile-suffix "_" simulation-id ".png"))))))

(defn run-simulations
  [{:keys
    [cell-size outfile-suffix output-csvs? fetch-temperature-method
     fetch-wind-speed-20ft-method fetch-wind-from-direction-method
     fetch-relative-humidity-method simulations output-layers] :as config}
   landfire-rasters envelope ignition-row
   ignition-col max-runtime temperature relative-humidity wind-speed-20ft
   wind-from-direction foliar-moisture ellipse-adjustment-factor ignition-raster
   multiplier-lookup perturbations]
  (mapv
   (fn [i]
     (let [initial-ignition-site (or ignition-raster
                                     [(ignition-row i) (ignition-col i)])
           temperature           (if fetch-temperature-method temperature (temperature i))
           wind-speed-20ft       (if fetch-wind-speed-20ft-method wind-speed-20ft (wind-speed-20ft i))
           wind-from-direction   (if fetch-wind-from-direction-method wind-from-direction (wind-from-direction i))
           relative-humidity     (if fetch-relative-humidity-method relative-humidity (relative-humidity i))]
       (if-let [fire-spread-results (run-fire-spread
                                     {:max-runtime               (max-runtime i)
                                      :cell-size                 cell-size
                                      :landfire-layers           landfire-rasters
                                      :wind-speed-20ft           wind-speed-20ft
                                      :wind-from-direction       wind-from-direction
                                      :temperature               temperature
                                      :relative-humidity         relative-humidity
                                      :foliar-moisture           (* 0.01 (foliar-moisture i))
                                      :ellipse-adjustment-factor (ellipse-adjustment-factor i)
                                      :num-rows                  (m/row-count (:fuel-model landfire-rasters))
                                      :num-cols                  (m/column-count (:fuel-model landfire-rasters))
                                      :multiplier-lookup         multiplier-lookup
                                      :perturbations             (when perturbations
                                                                   (perturbations i))}
                                     initial-ignition-site)]
         (do
           (if output-layers
             (process-output-layers-timestep! config fire-spread-results envelope i)
             (process-output-layers! config fire-spread-results envelope i))
           (when output-csvs?
             (merge
              {:ignition-row              (ignition-row i)
               :ignition-col              (ignition-col i)
               :ignition-raster           ignition-raster
               :max-runtime               (max-runtime i)
               :temperature               temperature
               :relative-humidity         relative-humidity
               :wind-speed-20ft           wind-speed-20ft
               :wind-from-direction       wind-from-direction
               :foliar-moisture           (foliar-moisture i)
               :ellipse-adjustment-factor (ellipse-adjustment-factor i)}
              (summarize-fire-spread-results fire-spread-results cell-size))))
         (when output-csvs?
           {:ignition-row               (ignition-row i)
            :ignition-col               (ignition-col i)
            :ignition-raster            ignition-raster
            :max-runtime                (max-runtime i)
            :temperature                temperature
            :relative-humidity          relative-humidity
            :wind-speed-20ft            wind-speed-20ft
            :wind-from-direction        wind-from-direction
            :foliar-moisture            (foliar-moisture i)
            :ellipse-adjustment-factor  (ellipse-adjustment-factor i)
            :fire-size                  0.0
            :flame-length-mean          0.0
            :flame-length-stddev        0.0
            :fire-line-intensity-mean   0.0
            :fire-line-intensity-stddev 0.0}))))
   (range simulations)))

(defn write-csv-outputs
  [output-csvs? output-filename results-table]
  (when output-csvs?
    (with-open [out-file (io/writer output-filename)]
      (->> results-table
           (sort-by #(vector (:ignition-row %) (:ignition-col %)))
           (mapv (fn [{:keys [ignition-row ignition-col max-runtime temperature relative-humidity wind-speed-20ft
                              wind-from-direction foliar-moisture ellipse-adjustment-factor fire-size flame-length-mean
                              flame-length-stddev fire-line-intensity-mean fire-line-intensity-stddev]}]
                   [ignition-row
                    ignition-col
                    max-runtime
                    temperature
                    relative-humidity
                    wind-speed-20ft
                    wind-from-direction
                    foliar-moisture
                    ellipse-adjustment-factor
                    fire-size
                    flame-length-mean
                    flame-length-stddev
                    fire-line-intensity-mean
                    fire-line-intensity-stddev]))
           (cons ["ignition-row" "ignition-col" "max-runtime" "temperature" "relative-humidity" "wind-speed-20ft"
                  "wind-from-direction" "foliar-moisture" "ellipse-adjustment-factor" "fire-size" "flame-length-mean"
                  "flame-length-stddev" "fire-line-intensity-mean" "fire-line-intensity-stddev"])
           (csv/write-csv out-file)))))

(defn get-envelope
  [config landfire-layers]
  (let [{:keys [upperleftx upperlefty width height scalex scaley]} (landfire-layers :elevation)]
    (make-envelope (:srid config)
                   upperleftx
                   (+ upperlefty (* height scaley))
                   (* width scalex)
                   (* -1.0 height scaley))))

(defn get-weather [config rand-generator weather-type weather-layers]
  (if (contains? weather-layers weather-type)
    (get-in weather-layers [weather-type :matrix])
    (draw-samples rand-generator (:simulations config) (config weather-type))))

(defn fetch-weather-layers [config]
  (reduce (fn [acc layer]
            (let [fetch-method (keyword (str/join "-" ["fetch" (name layer) "method"]))]
              (if (contains? config fetch-method)
                (assoc acc layer (fetch/weather config layer))
                acc)))
          {}
          spec/weather-names))

(defn create-multiplier-lookup
  [{:keys [cell-size] :as config} weather-layers]
  (reduce-kv (fn [acc k {:keys [scalex]}]
               (assoc acc k (int (quot (m->ft scalex) cell-size))))
             {}
             weather-layers))

(defn print-error [err config]
  (prn "CONFIG ERROR for:")
  (clojure.pprint/pprint config)
  (prn err))

(defn -main
  [& config-files]
  (doseq [config-file config-files]
    (let [config (edn/read-string (slurp config-file))]
      (if (s/valid? ::spec/config config)
        (let [landfire-layers   (fetch-landfire-layers config)
              landfire-rasters  (into {}
                                      (map (fn [[layer info]] [layer (:matrix info)]))
                                      landfire-layers)
              ignition-raster   (fetch/initial-ignition-layers config)
              weather-layers    (fetch-weather-layers config)
              multiplier-lookup (create-multiplier-lookup config weather-layers)
              envelope          (get-envelope config landfire-layers)
              simulations       (:simulations config)
              rand-generator    (if-let [seed (:random-seed config)]
                                  (Random. seed)
                                  (Random.))]
          (when (:output-landfire-inputs? config)
            (doseq [[layer matrix] landfire-rasters]
              (-> (matrix-to-raster (name layer) matrix envelope)
                  (write-raster (str (name layer) (:outfile-suffix config) ".tif")))))
          (->> (run-simulations
                config
                landfire-rasters
                envelope
                (draw-samples rand-generator simulations (:ignition-row config))
                (draw-samples rand-generator simulations (:ignition-col config))
                (draw-samples rand-generator simulations (:max-runtime config))
                (get-weather config rand-generator :temperature weather-layers)
                (get-weather config rand-generator :relative-humidity weather-layers)
                (get-weather config rand-generator :wind-speed-20ft weather-layers)
                (get-weather config rand-generator :wind-from-direction weather-layers)
                (draw-samples rand-generator simulations (:foliar-moisture config))
                (draw-samples rand-generator simulations (:ellipse-adjustment-factor config))
                ignition-raster
                multiplier-lookup
                (perturbation/draw-samples rand-generator simulations (:perturbations config)))
               (write-csv-outputs
                (:output-csvs? config)
                (str "summary_stats" (:outfile-suffix config) ".csv"))))
        (s/explain ::spec/config config)))))
;; command-line-interface ends here
