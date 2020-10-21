(ns gridfire.config-test
  (:require [gridfire.config :as config]
            [clojure.test :refer [deftest is testing]]))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def resources-path "test/gridfire/resources/")

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [filename]
  (str resources-path filename))

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest convert-test
  (is (= 42 (config/convert "42")))

  (is (= 42.0 (config/convert "42.0")))

  (is (= 42.0 (config/convert "42.")))

  (is (= -42.0 (config/convert "-42.0")))

  (is (= true (config/convert ".TRUE.")))

  (is (= false (config/convert ".FALSE.")))

  (is (= "some/directory" (config/convert "'some/directory'"))))

(deftest read-data-test
  (let [config (config/build-edn (->> (in-file-path "sample-elmfire.data")
                                      slurp
                                      config/parse))]

    (is (= config {:cell-size                 30.0,
                   :foliar-moisture           90.0,
                   :wind-speed-20ft           "./fuels_and_topography/ws_to_sample.tif",
                   :outfile-suffix            "",
                   :random-seed               1234567890,
                   :ellipse-adjustment-factor 1.0,
                   :output-landfire-inputs?   true,
                   :fetch-layer-method        :geotiff,
                   :max-runtime               180.0,
                   :landfire-layers
                   {:aspect             "./fuels_and_topography/asp.tif",
                    :canopy-base-height "./fuels_and_topography/cbh.tif",
                    :canopy-cover       "./fuels_and_topography/cc.tif",
                    :canopy-height      "./fuels_and_topography/ch.tif",
                    :crown-bulk-density "./fuels_and_topography/cbd.tif",
                    :fuel-model         "./fuels_and_topography/fbfm40.tif",
                    :slope              "./fuels_and_topography/slp.tif",
                    :elevation          "./fuels_and_topography/dem.tif"},
                   :relative-humidity         "./fuels_and_topography/rh_to_sample.tif",
                   :wind-from-direction       "./fuels_and_topography/wd_to_sample.tif",
                   :output-geotiffs?          true,
                   :simulations               10,
                   :output-csvs?              true,
                   :output-pngs?              true,
                   :ignition-layer            "./fuels_and_topography/phi.tif",
                   :temperature               "./fuels_and_topography/tmpf_to_sample.tif",
                   :fetch-ignition-method     :geotiff}))))
