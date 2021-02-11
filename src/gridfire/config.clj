(ns gridfire.config
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [gridfire.crown-fire :refer [m->ft]]))

;;-----------------------------------------------------------------------------
;; Util
;;-----------------------------------------------------------------------------

(def ^:dynamic elmfire-file-path "")

(defn file-path [directory file-name]
  (let [directory (subs directory 1 (count directory))]
    (str elmfire-file-path directory "/" file-name ".tif")))

(def regex-for-array-item #"^[A-Z0-9\_]+\(\d+\)")

(defn convert-val [s]
  (cond
    (re-matches #"^-?[0-9]\d*\.(\d+)?$" s) (Double/parseDouble s)
    (re-matches #"^\d+$" s)                (Integer/parseInt s)
    (re-matches #".TRUE." s)               true
    (re-matches #".FALSE." s)              false
    :else                                  (subs s 1 (dec (count s)))))

(defn convert-key [s]
  (if (re-matches regex-for-array-item s)
    (str/join "-" (str/split s #"[\(-\)]"))
    s))

(defn parse [s]
  (->> (str/split s #"\n")
       (filter #(str/includes? % "="))
       (mapcat #(str/split % #"="))
       (map str/trim)
       (apply hash-map)
       (reduce-kv (fn [m k v]
                    (assoc m (convert-key k) (convert-val v)))
                  {})))

(defn sec->min
  [seconds]
  (int (/ seconds 60)))

;;-----------------------------------------------------------------------------
;; Landfire
;;-----------------------------------------------------------------------------

(defn process-landfire-layers
  [{:strs [ASP_FILENAME CBH_FILENAME CC_FILENAME CH_FILENAME CBD_FILENAME
           FBFM_FILENAME SLP_FILENAME DEM_FILENAME FUELS_AND_TOPOGRAPHY_DIRECTORY]}
   _
   config]
  (let [dir FUELS_AND_TOPOGRAPHY_DIRECTORY]
    (merge config
           {:landfire-layers {:aspect             {:type   :geotiff
                                                   :source (file-path dir ASP_FILENAME)}
                              :canopy-base-height {:type   :geotiff
                                                   :source (file-path dir CBH_FILENAME)}
                              :canopy-cover       {:type   :geotiff
                                                   :source (file-path dir CC_FILENAME)}
                              :canopy-height      {:type   :geotiff
                                                   :source (file-path dir CH_FILENAME)}
                              :crown-bulk-density {:type   :geotiff
                                                   :source (file-path dir CBD_FILENAME)}
                              :elevation          {:type   :geotiff
                                                   :source (file-path dir DEM_FILENAME)}
                              :fuel-model         {:type   :geotiff
                                                   :source (file-path dir FBFM_FILENAME)}
                              :slope              {:type   :geotiff
                                                   :source (file-path dir SLP_FILENAME)}}})))

;;-----------------------------------------------------------------------------
;; Ignition
;;-----------------------------------------------------------------------------

(defn process-ignition
  [{:strs [PHI_FILENAME FUELS_AND_TOPOGRAPHY_DIRECTORY]}
   _
   config]
  (let [dir FUELS_AND_TOPOGRAPHY_DIRECTORY]
    (merge config
           {:ignition-layer {:type        :geotiff
                             :source      (file-path dir PHI_FILENAME)
                             :burn-values {:burned   -1.0
                                           :unburned 1.0}}})))


;;-----------------------------------------------------------------------------
;; Weather
;;-----------------------------------------------------------------------------

(defn process-weather
  [{:strs [TMP_FILENAME RH_FILENAME WS_FILENAME
           WD_FILENAME WEATHER_DIRECTORY]}
   _
   config]
  (let [dir    WEATHER_DIRECTORY
        layers {:temperature         {:type   :geotiff
                                      :source (file-path dir "tmpf")}
                :relative-humidity   {:type   :geotiff
                                      :source (file-path dir "rh")}
                :wind-speed-20ft     {:type   :geotiff
                                      :source (file-path dir WS_FILENAME)}
                :wind-from-direction {:type   :geotiff
                                      :source (file-path dir WD_FILENAME)}}]
    (merge config
           layers)))


;;-----------------------------------------------------------------------------
;; Output
;;-----------------------------------------------------------------------------

(defn process-output
  [{:strs [CALCULATE_BURN_PROBABILITY DTDUMP DUMP_BURN_PROBABILITY_AT_DTDUMP
           OUTPUTS_DIRECTORY]}
   {:keys [verbose]} config]
  (let [burn-probability (when DUMP_BURN_PROBABILITY_AT_DTDUMP
                           {:burn-probability (sec->min DTDUMP)})]
    (merge config
           burn-probability
           {:outfile-suffix          ""
            :output-landfire-inputs? false
            :output-geotiffs?        true
            :output-binary?          true
            :output-directory        (str elmfire-file-path (subs OUTPUTS_DIRECTORY 1))
            :output-pngs?            (if verbose true false)
            :output-csvs?            true})))

;;-----------------------------------------------------------------------------
;; Perturbations
;;-----------------------------------------------------------------------------

(def elmfire->gridfire
  "A mapping of Elmfire string names to Gridfire keywords"
  {"CBH"    :canopy-base-height
   "CC"     :canopy-cover
   "CH"     :canopy-height
   "CBD"    :crown-bulk-density
   "WS"     :wind-speed-20ft
   "WD"     :wind-direction
   "GLOBAL" :global
   "PIXEL"  :pixel})

(defn perturbation-info
  [config index]
  {:spatial-type (->> (str/join "-" ["SPATIAL_PERTURBATION" index])
                      (get config)
                      (get elmfire->gridfire))
   :range        [(get config (str/join "-" ["PDF_LOWER_LIMIT" index]))
                  (get config (str/join "-" ["PDF_UPPER_LIMIT" index]))]})

(defn perturbation-key [config index]
  (->> (str/join "-" ["RASTER_TO_PERTURB" index])
       (get config)
       (get elmfire->gridfire)))

(defn extract-perturbations
  [{:strs [NUM_RASTERS_TO_PERTURB] :as config}]
  (when (pos? NUM_RASTERS_TO_PERTURB)
    (into {}
          (map (fn [index]
                 (when-let [key (perturbation-key config index)]
                   [key (perturbation-info config index)]))
               (range 1 (inc NUM_RASTERS_TO_PERTURB))))))

(defn process-perturbations
  [{:strs [NUM_RASTERS_TO_PERTURB] :as data} options config]
  (let [perturbations (extract-perturbations data)]
    (when (seq perturbations)
      (merge config
             {:perturbations perturbations}))))

;;-----------------------------------------------------------------------------
;; Spotting
;;-----------------------------------------------------------------------------


(defn- extract-fuel-range [s]
  (mapv #(Integer/parseInt %) (str/split (re-find #"\d+:\d+" s) #":")))

(defn extract-surface-spotting-percents
  [{:keys [data]}]
  (if-let [SURFACE_FIRE_SPOTTING_PERCENT (get data "SURFACE_FIRE_SPOTTING_PERCENT(:)")]
    [[[1 204] SURFACE_FIRE_SPOTTING_PERCENT]]
    (let [kys (filter #(str/includes? (name %) "SURFACE_FIRE_SPOTTING_PERCENT") (keys data))]
      (reduce-kv (fn [acc k v]
                   (conj acc (extract-fuel-range k) v))
                 []
                 (select-keys data kys)))))

(defn extract-global-surface-spotting-percents
  [{:strs
    [GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN
     GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX
     GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT
     ENABLE_SPOTTING] :as data}]
  (if (or GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN)
    (if ENABLE_SPOTTING
      [[[1 204] [GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX]]]
      [[[1 204] GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT]])
    (extract-surface-spotting-percents data)))

(defn extract-crown-fire-spotting-percent
  [{:strs [CROWN_FIRE_SPOTTING_PERCENT_MIN CROWN_FIRE_SPOTTING_PERCENT_MAX
           CROWN_FIRE_SPOTTING_PERCENT ENABLE_SPOTTING]}]
  (if ENABLE_SPOTTING
    [CROWN_FIRE_SPOTTING_PERCENT_MIN CROWN_FIRE_SPOTTING_PERCENT_MAX]
    CROWN_FIRE_SPOTTING_PERCENT))

(defn extract-num-firebrands
  [{:strs [NEMBERS NEMBERS_MIN NEMBERS_MIN_LO NEMBERS_MIN_HI NEMBERS_MAX
           NEMBERS_MAX_LO NEMBERS_MAX_HI ENABLE_SPOTTING]}]

  (if ENABLE_SPOTTING
    {:lo (if NEMBERS_MIN_LO [NEMBERS_MIN_LO NEMBERS_MIN_HI] NEMBERS_MIN)
     :hi (if NEMBERS_MAX_LO [NEMBERS_MAX_LO NEMBERS_MAX_HI] NEMBERS_MAX)}
    NEMBERS))

(defn process-spotting
  [{:strs [ENABLE_SPOTTING ENABLE_SURFACE_FIRE_SPOTTING CRITICAL_SPOTTING_FIRELINE_INTENSITY] :as data} _ config]
  (if ENABLE_SPOTTING
    (let [spotting-config (cond-> {:spotting {:ambient-gas-density         1.1
                                              :crown-fire-spotting-percent (extract-crown-fire-spotting-percent data)
                                              :num-firebrands              (extract-num-firebrands data)
                                              :specific-heat-gas           1121.0}}

                            ENABLE_SURFACE_FIRE_SPOTTING
                            (assoc-in [:spotting :surface-fire-spotting]
                                      {:spotting-percent             (extract-global-surface-spotting-percents data)
                                       :critical-fire-line-intensity CRITICAL_SPOTTING_FIRELINE_INTENSITY}))]
      (merge config spotting-config))
    config))


;;-----------------------------------------------------------------------------
;; Main
;;-----------------------------------------------------------------------------

(defn build-edn
  [{:strs
    [COMPUTATIONAL_DOMAIN_CELLSIZE SIMULATION_TSTOP NUM_ENSEMBLE_MEMBERS
     A_SRS FOLIAR_MOISTURE_CONTENT SEED] :as d}
   options]
  (let [data (into (sorted-map ) d)]
    (->> {:cell-size                 (m->ft COMPUTATIONAL_DOMAIN_CELLSIZE)
          :srid                      A_SRS
          :max-runtime               (sec->min SIMULATION_TSTOP)
          :simulations               NUM_ENSEMBLE_MEMBERS
          :random-seed               SEED
          :foliar-moisture           FOLIAR_MOISTURE_CONTENT
          :ellipse-adjustment-factor 1.0}
         (process-landfire-layers data options)
         (process-ignition data options)
         (process-weather data options)
         (process-output data options)
         (process-perturbations data options)
         (process-spotting data options))))

(defn write-config [config-params]
  (let [file-name "gridfire.edn"]
    (println "Created Config file:" file-name)
    (println "Directory:" elmfire-file-path)
    (spit (str/join "/" [elmfire-file-path file-name])
          (with-out-str (pprint/pprint config-params)))))

(defn process-options
  [{:keys [config-file verbose] :as options}]
  (let [data (parse (slurp config-file))]
    (build-edn data options)))

(def cli-options
  [["-c" "--config-file FILE" "Path to an data file containing a map of simulation configs"]
   ["-v" "--verbose" "Flag for controlling outputs"]])

(defn -main
  [& args]
  (println (str "Converting configuration file to one that Gridfire accepts."))
  (let [{:keys [options summary errors]} (parse-opts args cli-options)]
    (if (or (seq errors) (empty? options))
      (do
        (when (seq errors)
          (run! println errors)
          (newline))
        (println (str "Usage:\n" summary)))
      (binding [elmfire-file-path (str/replace (:config-file options) #"/elmfire.data" "")]
       (write-config (process-options options))))))
