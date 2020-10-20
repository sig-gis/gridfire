(ns gridfire.config
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

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

(defn build-config
  [{:keys [ASP_FILENAME  CBH_FILENAME CC_FILENAME  CH_FILENAME CBD_FILENAME
           FBFM_FILENAME SLP_FILENAME DEM_FILENAME COMPUTATIONAL_DOMAIN_CELLSIZE
           MAX_RUNTIME STOCHASTIC_TMP_FILENAME STOCHASTIC_RH_FILENAME
           STOCHASTIC_WS_FILENAME STOCHASTIC_WD_FILENAME FOLIAR_MOISTURE_CONTENT
           FOLIAR_MOISTURE_CONTENT]}
   {:keys []}]
  {:fetch-layer-method        :geotiff
   :landfire-layers           {:aspect             ASP_FILENAME
                               :canopy-base-height CBH_FILENAME
                               :canopy-cover       CC_FILENAME
                               :canopy-height      CH_FILENAME
                               :crown-bulk-density CBD_FILENAME
                               :fuel-model         FBFM_FILENAME
                               :slope              SLP_FILENAME
                               :elevation          DEM_FILENAME}
   :cell-size                 COMPUTATIONAL_DOMAIN_CELLSIZE
   :fetch-ignition-method     :geotiff
   ;; :ignition-layers ;;TODO
   :max-runtime               MAX_RUNTIME
   :temperature               STOCHASTIC_TMP_FILENAME
   :relative-humidity         STOCHASTIC_RH_FILENAME
   :wind-speed-20ft           STOCHASTIC_WS_FILENAME
   :wind-from-direction       STOCHASTIC_WD_FILENAME
   :foliar-moisture           FOLIAR_MOISTURE_CONTENT
   :ellipse-adjustment-factor 1.0

   :simulations 10
   :random-seed 1234567890 ;; long value (optional)

   :outfile-suffix          "_tile_100"
   :output-landfire-inputs? true
   :output-geotiffs?        true
   :output-pngs?            true
   :output-csvs?            true
   })

(defn -main
  [data-file ctl-file]
  (let [data          (parse (slurp data-file))
        ctl           (parse (slurp ctl-file))
        output-config (build-config data ctl)]
    ;; ctl
    ;; data
    ;; output-config
    (spit "generated-config.edn" (with-out-str (pprint/pprint output-config)))
    ))

;; {:inputs  {:fuels-and-topography-directory "./fuels_and_topogrphy"
;;            :asp-filename                   "asp"
;;            :cbd-filename                   "cbd"}
;;  :outputs {:outputs-directory  "./outputs"
;;            :convert_to_geotiff false}}
