(ns gridfire.config-test
  (:require [gridfire.config :as config]
            [gridfire.crown-fire :refer [m->ft]]
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
                                      config/parse)
                                 nil)]

    (is (= config {:fetch-layer-method        :geotiff,
                   :landfire-layers           {:aspect             "./fuels_and_topography/asp.tif",
                                               :canopy-base-height "./fuels_and_topography/cbh.tif",
                                               :canopy-cover       "./fuels_and_topography/cc.tif",
                                               :canopy-height      "./fuels_and_topography/ch.tif",
                                               :crown-bulk-density "./fuels_and_topography/cbd.tif",
                                               :fuel-model         "./fuels_and_topography/fbfm40.tif",
                                               :slope              "./fuels_and_topography/slp.tif",
                                               :elevation          "./fuels_and_topography/dem.tif"},
                   :cell-size                 (m->ft 30.0),
                   :fetch-ignition-method     :geotiff,
                   :ignition-layer            "./fuels_and_topography/phi.tif",
                   :max-runtime               180.0,
                   :temperature               "./fuels_and_topography/tmpf_to_sample.tif",
                   :relative-humidity         "./fuels_and_topography/rh_to_sample.tif",
                   :wind-speed-20ft           "./fuels_and_topography/ws_to_sample.tif",
                   :wind-from-direction       "./fuels_and_topography/wd_to_sample.tif",
                   :ellipse-adjustment-factor 1.0,
                   :foliar-moisture           90.0,
                   :simulations               1000,
                   :random-seed               2020,
                   :outfile-suffix            "",
                   :output-geotiffs?          false,
                   :output-csvs?              false,
                   :output-pngs?              false,
                   :output-landfire-inputs?   false,
                   }))))
