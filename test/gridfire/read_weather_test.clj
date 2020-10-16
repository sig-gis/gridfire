(ns gridfire.read-weather-test
  (:require [clojure.core.matrix :as m]
            [clojure.java.jdbc :as jdbc]
            [gridfire.cli :as cli]
            [gridfire.magellan-bridge :as mb]
            [clojure.test :refer [deftest is]]
            [gridfire.cli :as cli]
            [gridfire.fetch :as fetch]
            [magellan.core :refer [read-raster]]
            [mikera.vectorz.core :as v])
  (:import java.util.Random))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def resources-path "test/gridfire/resources/weather-test/")

(def db-spec {:classname   "org.postgresql.Driver"
              :subprotocol "postgresql"
              :subname     "//localhost:5432/gridfire_test"
              :user        "gridfire_test"
              :password    "gridfire_test"})

(def test-config-base
  {:db-spec     db-spec
   :simulations 1
   :random-seed 1234567890})

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [filename]
  (str resources-path filename))

(defn run-simulation [config]
  (let [simulations      (:simulations config)
        rand-generator   (if-let [seed (:random-seed config)]
                           (Random. seed)
                           (Random.))
        landfire-layers  (cli/fetch-landfire-layers config)
        landfire-matrix  (into {} (map (fn [[layer-name info]] [layer-name (:matrix info)])) landfire-layers)
        ignition-layers  (fetch/initial-ignition-layers config)
        ignition-rasters (into {} (map (fn [[layer-name info]] [layer-name (:matrix info)])) ignition-layers)]
    (cli/run-simulations
     simulations
     landfire-matrix
     (cli/get-envelope config landfire-layers)
     (:cell-size config)
     (cli/draw-samples rand-generator simulations (:ignition-row config))
     (cli/draw-samples rand-generator simulations (:ignition-col config))
     (cli/draw-samples rand-generator simulations (:max-runtime config))
     (cli/get-weather config rand-generator (:elevation landfire-layers) :temperature)
     (cli/get-weather config rand-generator (:elevation landfire-layers) :relative-humidity)
     (cli/get-weather config rand-generator (:elevation landfire-layers) :wind-speed-20ft)
     (cli/get-weather config rand-generator (:elevation landfire-layers) :wind-from-direction)
     (cli/draw-samples rand-generator simulations (:foliar-moisture config))
     (cli/draw-samples rand-generator simulations (:ellipse-adjustment-factor config))
     (:outfile-suffix config)
     (:output-geotiffs? config)
     (:output-pngs? config)
     (:output-csvs? config)
     ignition-rasters
     config)))

(def raster-for-resample
  (mb/geotiff-raster-to-matrix (in-file-path "asp.tif")))

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest fetch-temperature-test
  (let [geotiff-file    "tmpf_to_sample.tif"
        geotiff-config  (merge test-config-base
                               {:fetch-temperature-method :geotiff
                                :temperature              (in-file-path geotiff-file)})
        postgis-table   "weather.tmpf WHERE rid=1"
        postgis-config  (merge test-config-base
                               {:fetch-temperature-method :postgis
                                :temperature              postgis-table})
        geotiff-results (fetch/weather geotiff-config raster-for-resample :temperature)
        postgis-results (fetch/weather postgis-config raster-for-resample :temperature)]

    (is (every? m/matrix? geotiff-results))

    (is (every? m/matrix? postgis-results))

    (is (= geotiff-results postgis-results))

    (let [numbands (count (:bands (read-raster (in-file-path geotiff-file))))]
      (is (= numbands (m/dimension-count geotiff-results 0))))

    (let [results  (jdbc/with-db-connection [conn (:db-spec test-config-base)]
                     (jdbc/query conn [(str "SELECT (ST_Metadata(rast)).numbands FROM " postgis-table)]))
          numbands (:numbands (first results))]
      (is (= numbands (m/dimension-count geotiff-results 0))))))

(deftest fetch-relative-humidity-test
  (let [geotiff-file    "rh_to_sample.tif"
        geotiff-config  (merge test-config-base
                               {:fetch-relative-humidity-method :geotiff
                                :relative-humidity              (in-file-path geotiff-file)})
        postgis-table   "weather.rh WHERE rid=1"
        postgis-config  (merge test-config-base
                               {:fetch-relative-humidity-method :postgis
                                :relative-humidity              postgis-table})
        geotiff-results (fetch/weather geotiff-config raster-for-resample :relative-humidity)
        postgis-results (fetch/weather postgis-config raster-for-resample :relative-humidity)]

    (is (every? m/matrix? geotiff-results))

    (is (every? m/matrix? postgis-results))

    (is (= geotiff-results postgis-results))

    (let [numbands (count (:bands (read-raster (in-file-path geotiff-file))))]
      (is (= numbands (m/dimension-count geotiff-results 0))))

    (let [results  (jdbc/with-db-connection [conn (:db-spec test-config-base)]
                     (jdbc/query conn [(str "SELECT (ST_Metadata(rast)).numbands FROM " postgis-table)]))
          numbands (:numbands (first results))]
      (is (= numbands (m/dimension-count postgis-results 0))))))

(deftest fetch-wind-speed-20ft-test
  (let [geotiff-file    "ws_to_sample.tif"
        geotiff-config  (merge test-config-base
                               {:fetch-wind-speed-20ft-method :geotiff
                                :wind-speed-20ft              (in-file-path geotiff-file)})
        postgis-table   "weather.ws WHERE rid=1"
        postgis-config  (merge test-config-base
                               {:fetch-wind-speed-20ft-method :postgis
                                :wind-speed-20ft              postgis-table})
        geotiff-results (fetch/weather geotiff-config raster-for-resample :wind-speed-20ft)
        postgis-results (fetch/weather postgis-config raster-for-resample :wind-speed-20ft)]

    (is (every? m/matrix? geotiff-results))

    (is (every? m/matrix? postgis-results))

    (is (= geotiff-results postgis-results))

    (let [numbands (count (:bands (read-raster (in-file-path geotiff-file))))]
      (is (= numbands (m/dimension-count geotiff-results 0))))

    (let [results  (jdbc/with-db-connection [conn (:db-spec test-config-base)]
                     (jdbc/query conn [(str "SELECT (ST_Metadata(rast)).numbands FROM " postgis-table)]))
          numbands (:numbands (first results))]
      (is (= numbands (m/dimension-count postgis-results 0))))))

(deftest fetch-wind-from-direction-test
  (let [geotiff-file    "wd_to_sample.tif"
        geotiff-config  (merge test-config-base
                               {:fetch-wind-from-direction-method :geotiff
                                :wind-from-direction              (in-file-path geotiff-file)})
        postgis-table   "weather.wd WHERE rid=1"
        postgis-config  (merge test-config-base
                               {:fetch-wind-from-direction-method :postgis
                                :wind-from-direction              postgis-table})
        geotiff-results (fetch/weather geotiff-config raster-for-resample :wind-from-direction)
        postgis-results (fetch/weather postgis-config raster-for-resample :wind-from-direction)]

    (is (every? m/matrix? geotiff-results))

    (is (every? m/matrix? postgis-results))

    (is (= geotiff-results postgis-results))

    (let [numbands (count (:bands (read-raster (in-file-path geotiff-file))))]
      (is (= numbands (m/dimension-count geotiff-results 0))))

    (let [results  (jdbc/with-db-connection [conn (:db-spec test-config-base)]
                     (jdbc/query conn [(str "SELECT (ST_Metadata(rast)).numbands FROM " postgis-table)]))
          numbands (:numbands (first results))]
      (is (= numbands (m/dimension-count postgis-results 0))))))


(deftest get-weather-from-geotiff-test
  (let [file           "ws_to_sample.tif"
        config         (merge test-config-base
                              {:fetch-wind-from-direction-method :geotiff
                               :wind-from-direction              (in-file-path file)})
        rand-generator (Random. (:random-seed config))
        results        (cli/get-weather config rand-generator raster-for-resample :wind-from-direction)]

    (is (vector results))

    (is (every? v/vectorz? results))))

(deftest get-weather-from-postgis-test
  (let [table          "weather.tmpf WHERE rid=1"
        config         (merge test-config-base
                              {:fetch-temperature-method :postgis
                               :temperature              table})
        rand-generator (Random. (:random-seed config))
        results        (cli/get-weather config rand-generator raster-for-resample :temperature)]

    (is (vector results))

    (is (every? v/vectorz? results))))

(deftest get-weather-from-range-test
  (let [config         (merge test-config-base
                              {:temperature [0 100]
                               :simulations 10})
        rand-generator (Random. (:random-seed config))
        results        (cli/get-weather config rand-generator raster-for-resample :temperature)]

    (is (vector results))

    (is (every? int? results))))

(deftest get-weather-from-list-test
  (let [tmp-list       '(0 10 20 30)
        config         (merge test-config-base
                              {:temperature tmp-list
                               :simulations 10})
        rand-generator (Random. (:random-seed config))
        results        (cli/get-weather config rand-generator raster-for-resample :temperature)]

    (is (vector results))

    (is (every? int? results))

    (is (= (set results) (set tmp-list)))))

(deftest get-weather-scalar-from-test
  (let [config         (merge test-config-base
                              {:temperature 42
                               :simulations 10})
        rand-generator (Random. (:random-seed config))
        results        (cli/get-weather config rand-generator raster-for-resample :temperature)]

    (is (vector results))

    (is (every? int? results))

    (is (apply = results))))

(deftest run-simulation-using-weather-raster-test
  (let [file   "tmpf_to_sample.tif"
        config (merge test-config-base
                      {:fetch-layer-method :geotiff
                       :landfire-layers    {:aspect             (in-file-path "asp.tif")
                                            :canopy-base-height (in-file-path "cbh.tif")
                                            :canopy-cover       (in-file-path "cc.tif")
                                            :canopy-height      (in-file-path "ch.tif")
                                            :crown-bulk-density (in-file-path "cbd.tif")
                                            :elevation          (in-file-path "dem.tif")
                                            :fuel-model         (in-file-path "fbfm40.tif")
                                            :slope              (in-file-path "slp.tif")}
                       :srid               "EPSG:32610"
                       :cell-size          98.425 ;; (feet)
                       :ignition-row       [10 90]
                       :ignition-col       [20 80]
                       :max-runtime        120

                       :fetch-temperature-method :geotiff
                       :temperature              (in-file-path "tmpf_to_sample.tif")

                       :relative-humidity         '(1 10 20)
                       :wind-speed-20ft           '(10 15 20)
                       :wind-from-direction       '(0 90 180 270)
                       :foliar-moisture           90
                       :ellipse-adjustment-factor 1.0
                       :output-csvs?              true})
        results (run-simulation config)]

    (is (every? some? results))))

