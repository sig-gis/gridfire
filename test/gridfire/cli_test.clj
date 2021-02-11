(ns gridfire.cli-test
  (:require [clojure.core.matrix :as m]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gridfire.cli :as cli]
            [gridfire.crown-fire :refer [m->ft]]
            [gridfire.fetch :as fetch]
            [gridfire.utils.test :as utils]
            [gridfire.perturbation :as perturbation]
            [gridfire.binary-output :as binary])
  (:import java.util.Random))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def resources-path "test/gridfire/resources/")

(def db-spec {:classname   "org.postgresql.Driver"
              :subprotocol "postgresql"
              :subname     "//localhost:5432/gridfire_test"
              :user        "gridfire_test"
              :password    "gridfire_test"})

(def test-config-base
  {:db-spec                   db-spec
   :landfire-layers           {:aspect             "landfire.asp WHERE rid=1"
                               :canopy-base-height "landfire.cbh WHERE rid=1"
                               :canopy-cover       "landfire.cc WHERE rid=1"
                               :canopy-height      "landfire.ch WHERE rid=1"
                               :crown-bulk-density "landfire.cbd WHERE rid=1"
                               :fuel-model         "landfire.fbfm40 WHERE rid=1"
                               :slope              "landfire.slp WHERE rid=1"
                               :elevation          "landfire.dem WHERE rid=1"}
   :srid                      "CUSTOM:900914"
   :cell-size                 98.425     ;; (feet)
   :ignition-row              [10 10]
   :ignition-col              [20 20]
   :max-runtime               60         ;; (minutes)
   :temperature               '(50)      ;; (degrees Fahrenheit)
   :relative-humidity         '(1)       ;; (%)
   :wind-speed-20ft           '(10)      ;; (miles/hour)
   :wind-from-direction       '(0)       ;; (degrees clockwise from north)
   :foliar-moisture           90         ;; (%)
   :ellipse-adjustment-factor 1.0        ;; (< 1.0 = more circular, > 1.0 = more elliptical)
   :simulations               1
   :random-seed               1234567890 ;; long value (optional)
   :output-csvs?              true})

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [filename]
  (str resources-path filename))

(defn run-simulation [config]
  (let [landfire-layers   (fetch/landfire-layers config)
        landfire-rasters  (into {} (map (fn [[layer info]] [layer (first (:matrix info))])) landfire-layers)
        ignition-layer    (fetch/ignition-layer config)
        weather-layers    (fetch/weather-layers config)
        multiplier-lookup (cli/create-multiplier-lookup config weather-layers)
        envelope          (cli/get-envelope config landfire-layers)
        simulations       (:simulations config)
        rand-generator    (if-let [seed (:random-seed config)] (Random. seed) (Random.))
        max-runtimes      (cli/draw-samples rand-generator simulations (:max-runtime config))
        num-rows          (m/row-count (:fuel-model landfire-rasters))
        num-cols          (m/column-count (:fuel-model landfire-rasters))
        burn-count-matrix (cli/initialize-burn-count-matrix config max-runtimes num-rows num-cols)]
    (cli/run-simulations
     config
     landfire-rasters
     envelope
     (cli/draw-samples rand-generator simulations (:ignition-row config))
     (cli/draw-samples rand-generator simulations (:ignition-col config))
     max-runtimes
     (cli/get-weather config rand-generator :temperature weather-layers)
     (cli/get-weather config rand-generator :relative-humidity weather-layers)
     (cli/get-weather config rand-generator :wind-speed-20ft weather-layers)
     (cli/get-weather config rand-generator :wind-from-direction weather-layers)
     (cli/draw-samples rand-generator simulations (:foliar-moisture config))
     (cli/draw-samples rand-generator simulations (:ellipse-adjustment-factor config))
     ignition-layer
     multiplier-lookup
     (perturbation/draw-samples rand-generator simulations (:perturbations config))
     burn-count-matrix)))

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(use-fixtures :once utils/with-temp-output-dir)

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest fetch-landfire-layers-old-config-test
  (is (some? (fetch/landfire-layers test-config-base))))

(deftest fetch-landfire-layers-test
  (testing "Fetching layers from postgis and geotiff files"
    (let [postgis-config {:db-spec         db-spec
                          :srid            "CUSTOM:900914"
                          :landfire-layers {:aspect             {:type   :postgis
                                                                 :source "landfire.asp WHERE rid=1"}
                                            :canopy-base-height {:type   :postgis
                                                                 :source "landfire.cbh WHERE rid=1"}
                                            :canopy-cover       {:type   :postgis
                                                                 :source "landfire.cc WHERE rid=1"}
                                            :canopy-height      {:type   :postgis
                                                                 :source "landfire.ch WHERE rid=1"}
                                            :crown-bulk-density {:type   :postgis
                                                                 :source "landfire.cbd WHERE rid=1"}
                                            :elevation          {:type   :postgis
                                                                 :source "landfire.dem WHERE rid=1"}
                                            :fuel-model         {:type   :postgis
                                                                 :source "landfire.fbfm40 WHERE rid=1"}
                                            :slope              {:type   :postgis
                                                                 :source "landfire.slp WHERE rid=1"}}}
          geotiff-config {:landfire-layers {:aspect             {:type   :geotiff
                                                                 :source (in-file-path "asp.tif")}
                                            :canopy-base-height {:type   :geotiff
                                                                 :source (in-file-path "cbh.tif")}
                                            :canopy-cover       {:type   :geotiff
                                                                 :source (in-file-path "cc.tif")}
                                            :canopy-height      {:type   :geotiff
                                                                 :source (in-file-path "ch.tif")}
                                            :crown-bulk-density {:type   :geotiff
                                                                 :source (in-file-path "cbd.tif")}
                                            :elevation          {:type   :geotiff
                                                                 :source (in-file-path "dem.tif")}
                                            :fuel-model         {:type   :geotiff
                                                                 :source (in-file-path "fbfm40.tif")}
                                            :slope              {:type   :geotiff
                                                                 :source (in-file-path "slp.tif")}}}
          postgis        (fetch/landfire-layers postgis-config)
          geotiff        (fetch/landfire-layers geotiff-config)]

      (is (= (get-in postgis [:aspect :matrix])
             (get-in geotiff [:aspect :matrix])))

      (is (= (get-in postgis [:canopy-cover :matrix])
             (get-in geotiff [:canopy-cover :matrix])))

      (is (= (get-in postgis [:canopy-height :matrix])
             (get-in geotiff [:canopy-height :matrix])))

      (is (= (get-in postgis [:crown-bulk-density :matrix])
             (get-in geotiff [:crown-bulk-density :matrix])))

      (is (= (get-in postgis [:elevation :matrix])
             (get-in geotiff [:elevation :matrix])))

      (is (= (get-in postgis [:fuel-model :matrix])
             (get-in geotiff [:fuel-model :matrix])))

      (is (= (get-in postgis [:slope :matrix])
             (get-in geotiff [:slope :matrix])))

      ;; TODO Add test for envelope
      )))

;;-----------------------------------------------------------------------------
;; Landfire Layer Tests
;;-----------------------------------------------------------------------------

(deftest run-simulation-test
  (testing "Running simulation with different ways to fetch landfire layers"
    (let [postgis-config  (merge test-config-base
                                 {:landfire-layers    {:aspect             {:type   :postgis
                                                                            :source "landfire.asp WHERE rid=1"}
                                                       :canopy-base-height {:type   :postgis
                                                                            :source "landfire.cbh WHERE rid=1"}
                                                       :canopy-cover       {:type   :postgis
                                                                            :source "landfire.cc WHERE rid=1"}
                                                       :canopy-height      {:type   :postgis
                                                                            :source "landfire.ch WHERE rid=1"}
                                                       :crown-bulk-density {:type   :postgis
                                                                            :source "landfire.cbd WHERE rid=1"}
                                                       :elevation          {:type   :postgis
                                                                            :source "landfire.dem WHERE rid=1"}
                                                       :fuel-model         {:type   :postgis
                                                                            :source "landfire.fbfm40 WHERE rid=1"}
                                                       :slope              {:type   :postgis
                                                                            :source "landfire.slp WHERE rid=1"}}
                                  :fetch-layer-method :postgis})
          geotiff-config  (merge test-config-base
                                 {:landfire-layers    {:aspect             {:type   :geotiff
                                                                            :source (in-file-path "asp.tif")}
                                                       :canopy-base-height {:type   :geotiff
                                                                            :source (in-file-path "cbh.tif")}
                                                       :canopy-cover       {:type   :geotiff
                                                                            :source (in-file-path "cc.tif")}
                                                       :canopy-height      {:type   :geotiff
                                                                            :source (in-file-path "ch.tif")}
                                                       :crown-bulk-density {:type   :geotiff
                                                                            :source (in-file-path "cbd.tif")}
                                                       :elevation          {:type   :geotiff
                                                                            :source (in-file-path "dem.tif")}
                                                       :fuel-model         {:type   :geotiff
                                                                            :source (in-file-path "fbfm40.tif")}
                                                       :slope              {:type   :geotiff
                                                                            :source (in-file-path "slp.tif")}}
                                  :fetch-layer-method :geotiff})
          simulations     (:simulations test-config-base)
          postgis-layers  (fetch/landfire-layers postgis-config)
          postgis-results (run-simulation postgis-config)

          geotiff-layers  (fetch/landfire-layers geotiff-config)
          geotiff-results (run-simulation geotiff-config)]

      (is (every? some? postgis-results))

      (is (every? some? geotiff-results))

      (is (= (set postgis-results) (set geotiff-results))))))

;;-----------------------------------------------------------------------------
;; Ignition Layer Tests
;;-----------------------------------------------------------------------------

(deftest geotiff-ignition-test
  (testing "Running simulation with ignition layers read from geotiff files"
    (let [geotiff-config (merge test-config-base
                                {:ignition-layer {:type   :geotiff
                                                  :source (in-file-path "ign.tif")}})
          results        (run-simulation geotiff-config)]

      (is (every? some? results)))))

(deftest postgis-ignition-test
  (testing "Running simulation with ignition layers read from Postgres database"
    (let [postgis-config (merge test-config-base
                                {:ignition-layer {:type   :postgis
                                                  :source "ignition.ign WHERE rid=1"}})
          results        (run-simulation postgis-config)]
      (is (every? some? results)))))

(deftest burn-value-test
  (testing "Running simulation with burned and unburned values different from Gridfire's definition"
    (let [postgis-config (merge test-config-base
                                {:ignition-layer {:type        :geotiff
                                                  :source      (in-file-path "ign-inverted.tif")
                                                  :burn-values {:burned   -1.0
                                                                :unburned 1.0}}})
          results        (run-simulation postgis-config)]
      (is (every? some? results)))))

;;-----------------------------------------------------------------------------
;; Weather Layer Tests
;;-----------------------------------------------------------------------------

(def landfire-layers-weather-test
  {:aspect             {:type   :geotiff
                        :source (in-file-path "weather-test/asp.tif")}
   :canopy-base-height {:type   :geotiff
                        :source (in-file-path "weather-test/cbh.tif")}
   :canopy-cover       {:type   :geotiff
                        :source (in-file-path "weather-test/cc.tif")}
   :canopy-height      {:type   :geotiff
                        :source (in-file-path "weather-test/ch.tif")}
   :crown-bulk-density {:type   :geotiff
                        :source (in-file-path "weather-test/cbd.tif")}
   :elevation          {:type   :geotiff
                        :source (in-file-path "weather-test/dem.tif")}
   :fuel-model         {:type   :geotiff
                        :source (in-file-path "weather-test/fbfm40.tif")}
   :slope              {:type   :geotiff
                        :source (in-file-path "weather-test/slp.tif")}})

(deftest run-simulation-using-temperature-raster-test
  (testing "Running simulation using temperature data from geotiff file"
    (let [config  (merge test-config-base
                         {:landfire-layers landfire-layers-weather-test
                          :max-runtime     120
                          :temperature     {:type   :geotiff
                                            :source (in-file-path "weather-test/tmpf_to_sample.tif")}})
          results (run-simulation config)]

      (is (every? some? results)))))

(deftest run-simulation-using-relative-humidity-raster-test
  (testing "Running simulation using relative humidity data from geotiff file"
    (let [config  (merge test-config-base
                         {:landfire-layers landfire-layers-weather-test
                          :max-runtime     120
                          :temperature     {:type   :geotiff
                                            :source (in-file-path "weather-test/rh_to_sample.tif")}})
          results (run-simulation config)]

      (is (every? some? results)))))

(deftest run-simulation-using-wind-speed-raster-test
  (testing "Running simulation using wind speed data from geotiff file"
    (let [config  (merge test-config-base
                         {:landfire-layers landfire-layers-weather-test
                          :max-runtime     120
                          :temperature     {:type   :geotiff
                                            :source (in-file-path "weather-test/ws_to_sample.tif")}})
          results (run-simulation config)]

      (is (every? some? results)))))

(deftest run-simulation-using-wind-direction-raster-test
  (testing "Running simulation using wind direction data from geotiff file"
    (let [config  (merge test-config-base
                         {:landfire-layers landfire-layers-weather-test
                          :max-runtime     120
                          :temperature     {:type   :geotiff
                                            :source (in-file-path "weather-test/wd_to_sample.tif")}})
          results (run-simulation config)]

      (is (every? some? results)))))

(deftest geotiff-landfire-weather-ignition
  (testing "Running simulation using landfire, weather, and ignition data from geotiff files"
    (let [config  (merge test-config-base
                         {:landfire-layers     landfire-layers-weather-test
                          :ignition-layer      {:type   :geotiff
                                                :source (in-file-path "weather-test/phi.tif")}
                          :temperature         {:type   :geotiff
                                                :source (in-file-path "weather-test/tmpf_to_sample.tif")}
                          :relative-humidity   {:type   :geotiff
                                                :source (in-file-path "weather-test/rh_to_sample.tif")}
                          :wind-speed-20ft     {:type   :geotiff
                                                :source (in-file-path "weather-test/ws_to_sample.tif")}
                          :wind-from-direction {:type   :geotiff
                                                :source (in-file-path "weather-test/wd_to_sample.tif")}
                          :max-runtime         120})
          results (run-simulation config)]

      (is (every? some? results)))))

(deftest run-simulation-using-lower-resolution-weather-test
  (testing "Running simulation using temperature data from geotiff file"
    (let [config  (merge test-config-base
                         {:cell-size       (m->ft 30)
                          :landfire-layers landfire-layers-weather-test
                          :temperature     {:type   :geotiff
                                            :source (in-file-path "weather-test/tmpf_to_sample_lower_res.tif")}
                          }
                         )
          results (run-simulation config)]

      (is (every? some? results)))))

(deftest multiplier-lookup-test
  (testing "constructing multiplier lookup for weather raster"
    (let [config         {:cell-size   (m->ft 30)
                          :temperature {:type   :geotiff
                                        :source (in-file-path "/weather-test/tmpf_to_sample_lower_res.tif")}}
          weather-layers (fetch/weather-layers config)
          lookup         (cli/create-multiplier-lookup config weather-layers)]

      (is (= {:temperature 10} lookup)))))

;;-----------------------------------------------------------------------------
;; Perturbation Tests
;;-----------------------------------------------------------------------------

(deftest run-simulation-with-landfire-perturbations
  (testing "with global perturbation value"
    (let [config  (merge test-config-base
                         {:perturbations {:canopy-height {:spatial-type :global
                                                          :range        [-1.0 1.0]}}})
          results (run-simulation config)]
      (is (every? some? results))))

  (testing "with pixel by pixel perturbation"
    (let [config  (merge test-config-base
                         {:perturbations {:canopy-height {:spatial-type :pixel
                                                          :range        [-1.0 1.0]}}})
          results (run-simulation config)]
      (is (every? some? results)))))

;;-----------------------------------------------------------------------------
;; Outputs
;;-----------------------------------------------------------------------------

(deftest binary-output-files-test
  (let [config         (merge test-config-base
                              {:output-binary?     true
                               :output-directory   "test/output"
                               :fetch-layer-method :postgis})
        _              (run-simulation config)
        binary-results (binary/read-matrices-as-binary (utils/out-file-path "toa_0001_00001.bin")
                                                       [:float :float :float :int])]
    (is (some? binary-results))))
