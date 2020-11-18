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
           {:fetch-layer-method :geotiff
            :landfire-layers    {:aspect             (file-path dir ASP_FILENAME)
                                 :canopy-base-height (file-path dir CBH_FILENAME)
                                 :canopy-cover       (file-path dir CC_FILENAME)
                                 :canopy-height      (file-path dir CH_FILENAME)
                                 :crown-bulk-density (file-path dir CBD_FILENAME)
                                 :elevation          (file-path dir DEM_FILENAME)
                                 :fuel-model         (file-path dir FBFM_FILENAME)
                                 :slope              (file-path dir SLP_FILENAME)}})))

;;-----------------------------------------------------------------------------
;; Ignition
;;-----------------------------------------------------------------------------

(defn process-ignition
  [{:strs [NUM_IGNITIONS PHI_FILENAME FUELS_AND_TOPOGRAPHY_DIRECTORY]}
   _
   config]
  (let [dir FUELS_AND_TOPOGRAPHY_DIRECTORY]
    (merge config
           {:fetch-ignition-method :geotiff
            :ignition-layer        {:path        (file-path dir PHI_FILENAME)
                                    :burn-values {:burned   -1.0
                                                  :unburned 1.0}}})))


;;-----------------------------------------------------------------------------
;; Weather
;;-----------------------------------------------------------------------------

(defn process-weather
  [{:strs [STOCHASTIC_TMP_FILENAME STOCHASTIC_RH_FILENAME STOCHASTIC_WS_FILENAME
           STOCHASTIC_WD_FILENAME FOLIAR_MOISTURE_CONTENT WEATHER_DIRECTORY]}
   _
   config]
  (let [dir           WEATHER_DIRECTORY
        to-fetch-kw   (fn [k] (keyword (str/join "-" ["fetch" (name k) "method"])))
        layers        {:temperature         (file-path dir STOCHASTIC_TMP_FILENAME)
                       :relative-humidity   (file-path dir STOCHASTIC_RH_FILENAME)
                       :wind-speed-20ft     (file-path dir STOCHASTIC_WS_FILENAME)
                       :wind-from-direction (file-path dir STOCHASTIC_WD_FILENAME)
                       :foliar-moisture     FOLIAR_MOISTURE_CONTENT}
        fetch-methods (reduce-kv (fn[acc k v]
                                   (assoc acc (to-fetch-kw k) :geotiff))
                                 {}
                                 layers)]
    (merge config
           layers
           fetch-methods)))


;;-----------------------------------------------------------------------------
;; Output
;;-----------------------------------------------------------------------------

(defn process-output
  [data {:keys [verbose]} config]
  (merge config
         {:outfile-suffix          ""
          :output-landfire-inputs? false
          :output-geotiffs?        true
          :output-pngs?            (if verbose true false)
          :output-csvs?            (if verbose true false)}))

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
;; Main
;;-----------------------------------------------------------------------------

(defn build-edn
  [{:strs
    [COMPUTATIONAL_DOMAIN_CELLSIZE SIMULATION_TSTOP NUM_ENSEMBLE_MEMBERS
     A_SRS
     SEED] :as data}
   options]
  (->> {:cell-size                 (m->ft COMPUTATIONAL_DOMAIN_CELLSIZE)
        :srid                      A_SRS
        :max-runtime               (sec->min SIMULATION_TSTOP)
        :simulations               NUM_ENSEMBLE_MEMBERS
        :random-seed               SEED
        :ellipse-adjustment-factor 1.0}
       (process-landfire-layers data options)
       (process-ignition data options)
       (process-weather data options)
       (process-output data options)
       (process-perturbations data options)))

(defn write-config [config-params]
  (let [file "gridfire.edn"]
    (println "Config file:" file)
    (spit file (with-out-str (pprint/pprint config-params)))))

(defn process-options
  [{:keys [config-file verbose] :as options}]
  (binding [elmfire-file-path (str/replace config-file #"/elmfire.data" "")]
    (let [data (parse (slurp config-file))]
      (build-edn data options))))

(def cli-options
  [["-c" "--config-file FILE" "Path to an data file containing a map of simulation configs"]
   ["-v" "--verbose" "Flag for controlling outputs"]])

(defn -main
  [& args]
  (println (str "Converting configuration file to one that Gridfire accepts."))
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    (if (or (seq errors) (empty? options))
      (do
        (when (seq errors)
          (run! println errors)
          (newline))
        (println (str "Usage:\n" summary)))
      (write-config (process-options options)))))
