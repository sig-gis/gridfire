(ns gridfire.config
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn file-path [directory file-name]
  (str directory "/" file-name ".tif"))

(defn convert [s]
  (cond
    (re-matches #"^-?[0-9]\d*\.(\d+)?$" s) (Float/parseFloat s)
    (re-matches #"^\d+$" s)                (Integer/parseInt s)
    (re-matches #".TRUE." s)               true
    (re-matches #".FALSE." s)              false
    :else                                  (subs s 1 (dec (count s)))))

(defn parse [s]
  (->> s
       (#(clojure.string/split % #"\n"))
       (mapcat #(clojure.string/split % #"="))
       (map clojure.string/trim)
       (remove #(re-matches #"&.*" %))
       (remove #(re-matches #"/" %))
       (remove #(re-matches #"" %))
       (apply (partial assoc {}))
       (reduce-kv (fn [m k v]
                    (assoc m (keyword k) (convert v)))
                  {})))

(defn build-edn
  [{:keys [FUELS_AND_TOPOGRAPHY_DIRECTORY ASP_FILENAME CBH_FILENAME CC_FILENAME
           CH_FILENAME CBD_FILENAME FBFM_FILENAME SLP_FILENAME DEM_FILENAME
           COMPUTATIONAL_DOMAIN_CELLSIZE PHI_FILENAME MAX_RUNTIME
           STOCHASTIC_TMP_FILENAME STOCHASTIC_RH_FILENAME STOCHASTIC_WS_FILENAME
           STOCHASTIC_WD_FILENAME FOLIAR_MOISTURE_CONTENT FOLIAR_MOISTURE_CONTENT]}]
  (let [dir FUELS_AND_TOPOGRAPHY_DIRECTORY]
    {:fetch-layer-method        :geotiff
     :landfire-layers           {:aspect             (file-path dir ASP_FILENAME)
                                 :canopy-base-height (file-path dir CBH_FILENAME)
                                 :canopy-cover       (file-path dir CC_FILENAME)
                                 :canopy-height      (file-path dir CH_FILENAME)
                                 :crown-bulk-density (file-path dir CBD_FILENAME)
                                 :fuel-model         (file-path dir FBFM_FILENAME)
                                 :slope              (file-path dir SLP_FILENAME)
                                 :elevation          (file-path dir DEM_FILENAME)}
     :cell-size                 COMPUTATIONAL_DOMAIN_CELLSIZE
     :fetch-ignition-method     :geotiff
     :ignition-layer            (file-path dir PHI_FILENAME)
     :max-runtime               MAX_RUNTIME
     :temperature               (file-path dir STOCHASTIC_TMP_FILENAME)
     :relative-humidity         (file-path dir STOCHASTIC_RH_FILENAME)
     :wind-speed-20ft           (file-path dir STOCHASTIC_WS_FILENAME)
     :wind-from-direction       (file-path dir STOCHASTIC_WD_FILENAME)
     :foliar-moisture           FOLIAR_MOISTURE_CONTENT
     :ellipse-adjustment-factor 1.0

     :simulations 10
     :random-seed 1234567890 ;; long value (optional)

     :outfile-suffix          ""
     :output-landfire-inputs? true
     :output-geotiffs?        true
     :output-pngs?            true
     :output-csvs?            true}))

(defn -main
  [data-file]
  (let [data          (parse (slurp data-file))
        output-config (build-edn data)]
    (spit "generated-config.edn" (with-out-str (pprint/pprint output-config)))))
