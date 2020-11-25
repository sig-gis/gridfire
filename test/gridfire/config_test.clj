(ns gridfire.config-test
  (:require [gridfire.config :as config]
            [gridfire.crown-fire :refer [m->ft]]
            [gridfire.spec.perturbations :as spec-p]
            [gridfire.spec.config :as spec]
            [clojure.spec.alpha :as s]
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
  (is (= 42 (config/convert-val "42")))

  (is (= 42.0 (config/convert-val "42.0")))

  (is (= 42.0 (config/convert-val "42.")))

  (is (= -42.0 (config/convert-val "-42.0")))

  (is (= true (config/convert-val ".TRUE.")))

  (is (= false (config/convert-val ".FALSE.")))

  (is (= "some/directory" (config/convert-val"'some/directory'"))))

(deftest extract-perturbations-test
  (let [config  (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse)
        results (config/extract-perturbations config)]

    (is (= {:crown-bulk-density {:spatial-type :global :range [-0.05 0.05]}
            :canopy-base-height {:spatial-type :global :range [-2.0 2.0]}
            :canopy-cover       {:spatial-type :global :range [-0.05 0.05]}
            :canopy-height      {:spatial-type :global :range [-5.0 5.0]}
            :wind-speed-20ft    {:spatial-type :global :range [-1.0 1.0]}
            :wind-direction     {:spatial-type :global :range [-7.5 7.5]}}
           results))))

(deftest read-data-test
  (let [config (config/build-edn (->> (in-file-path "sample-elmfire.data")
                                      slurp
                                      config/parse)
                                 nil)]

    (is (s/valid? ::spec/config config))

    (is (= config {:fetch-layer-method               :geotiff
                   :landfire-layers                  {:aspect             "/fuels_and_topography/asp.tif"
                                                      :canopy-base-height "/fuels_and_topography/cbh.tif"
                                                      :canopy-cover       "/fuels_and_topography/cc.tif"
                                                      :canopy-height      "/fuels_and_topography/ch.tif"
                                                      :crown-bulk-density "/fuels_and_topography/cbd.tif"
                                                      :fuel-model         "/fuels_and_topography/fbfm40.tif"
                                                      :slope              "/fuels_and_topography/slp.tif"
                                                      :elevation          "/fuels_and_topography/dem.tif"}
                   :cell-size                        (m->ft 30.0)
                   :fetch-ignition-method            :geotiff
                   :ignition-layer                   {:path        "/fuels_and_topography/phi.tif",
                                                      :burn-values {:burned -1.0, :unburned 1.0}}
                   :max-runtime                      4320
                   :temperature                      "/weather/tmpf_to_sample.tif"
                   :relative-humidity                "/weather/rh_to_sample.tif"
                   :wind-speed-20ft                  "/weather/ws_to_sample.tif"
                   :wind-from-direction              "/weather/wd_to_sample.tif"
                   :ellipse-adjustment-factor        1.0
                   :foliar-moisture                  90.0
                   :simulations                      1000
                   :random-seed                      2020
                   :outfile-suffix                   ""
                   :output-geotiffs?                 true
                   :output-csvs?                     false
                   :output-pngs?                     false
                   :output-landfire-inputs?          false
                   :perturbations                    {:crown-bulk-density {:spatial-type :global
                                                                           :range        [-0.05 0.05]}
                                                      :canopy-base-height {:spatial-type :global
                                                                           :range        [-2.0 2.0]}
                                                      :canopy-cover       {:spatial-type :global
                                                                           :range        [-0.05 0.05]}
                                                      :canopy-height      {:spatial-type :global
                                                                           :range        [-5.0 5.0]}
                                                      :wind-speed-20ft    {:spatial-type :global
                                                                           :range        [-1.0 1.0]}
                                                      :wind-direction     {:spatial-type :global
                                                                           :range        [-7.5 7.5]}}
                   :srid                             "EPSG:32610"
                   :fetch-temperature-method         :geotiff
                   :fetch-relative-humidity-method   :geotiff
                   :fetch-wind-speed-20ft-method     :geotiff
                   :fetch-wind-from-direction-method :geotiff
                   :burn-probability                 60}))))
