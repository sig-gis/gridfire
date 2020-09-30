(ns gridfire.cli-test
  (:require [clojure.core.matrix :as m]
            [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is testing]]
            [magellan.core :as mg]
            [gridfire.cli :as gf])
  (:import (java.util Random)))

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

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest fetch-landfire-layers-test
  (testing "Fetching layers from postgis and geotiff files"
    (let [postgis-config {:db-spec            db-spec
                          :srid               "CUSTOM:900914"
                          :landfire-layers    {:aspect             "landfire.asp WHERE rid=1"
                                               :canopy-base-height "landfire.cbh WHERE rid=1"
                                               :canopy-cover       "landfire.cc WHERE rid=1"
                                               :canopy-height      "landfire.ch WHERE rid=1"
                                               :crown-bulk-density "landfire.cbd WHERE rid=1"
                                               :elevation          "landfire.dem WHERE rid=1"
                                               :fuel-model         "landfire.fbfm40 WHERE rid=1"
                                               :slope              "landfire.slp WHERE rid=1"}
                          :fetch-layer-method :postgis}
          geotiff-config {:landfire-layers    {:aspect             (in-file-path "asp.tif")
                                               :canopy-base-height (in-file-path "cbh.tif")
                                               :canopy-cover       (in-file-path "cc.tif")
                                               :canopy-height      (in-file-path "ch.tif")
                                               :crown-bulk-density (in-file-path "cbd.tif")
                                               :elevation          (in-file-path "dem.tif")
                                               :fuel-model         (in-file-path "fbfm40.tif")
                                               :slope              (in-file-path "slp.tif")}
                          :fetch-layer-method :geotiff}
          postgis        (gf/fetch-landfire-layers postgis-config)
          geotiff        (gf/fetch-landfire-layers geotiff-config)]

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

(deftest run-simulation-test
  (testing "Running simulation with different ways to fetch layers"
    (let [postgis-config  (merge test-config-base
                                 {:landfire-layers    {:aspect             "landfire.asp WHERE rid=1"
                                                       :canopy-base-height "landfire.cbh WHERE rid=1"
                                                       :canopy-cover       "landfire.cc WHERE rid=1"
                                                       :canopy-height      "landfire.ch WHERE rid=1"
                                                       :crown-bulk-density "landfire.cbd WHERE rid=1"
                                                       :elevation          "landfire.dem WHERE rid=1"
                                                       :fuel-model         "landfire.fbfm40 WHERE rid=1"
                                                       :slope              "landfire.slp WHERE rid=1"}
                                  :fetch-layer-method :postgis})
          geotiff-config  (merge test-config-base
                                 {:landfire-layers    {:aspect             (in-file-path "asp.tif")
                                                       :canopy-base-height (in-file-path "cbh.tif")
                                                       :canopy-cover       (in-file-path "cc.tif")
                                                       :canopy-height      (in-file-path "ch.tif")
                                                       :crown-bulk-density (in-file-path "cbd.tif")
                                                       :elevation          (in-file-path "dem.tif")
                                                       :fuel-model         (in-file-path "fbfm40.tif")
                                                       :slope              (in-file-path "slp.tif")}
                                  :fetch-layer-method :geotiff})
          simulations     (:simulations test-config-base)
          rand-generator  (if-let [seed (:random-seed test-config-base)]
                            (Random. seed)
                            (Random.))
          postgis-layers  (gf/fetch-landfire-layers postgis-config)
          postgis-results (gf/run-simulations
                           simulations
                           (reduce (fn [acc [layer info]] (assoc acc layer (:matrix info)))
                                   {}
                                   postgis-layers)
                           (gf/get-envelope postgis-config postgis-layers)
                           (:cell-size test-config-base)
                           (gf/draw-samples rand-generator simulations (:ignition-row test-config-base))
                           (gf/draw-samples rand-generator simulations (:ignition-col test-config-base))
                           (gf/draw-samples rand-generator simulations (:max-runtime test-config-base))
                           (gf/draw-samples rand-generator simulations (:temperature test-config-base))
                           (gf/draw-samples rand-generator simulations (:relative-humidity test-config-base))
                           (gf/draw-samples rand-generator simulations (:wind-speed-20ft test-config-base))
                           (gf/draw-samples rand-generator simulations (:wind-from-direction test-config-base))
                           (gf/draw-samples rand-generator simulations (:foliar-moisture test-config-base))
                           (gf/draw-samples rand-generator simulations (:ellipse-adjustment-factor test-config-base))
                           (:outfile-suffix test-config-base)
                           (:output-geotiffs? test-config-base)
                           (:output-pngs? test-config-base)
                           (:output-csvs? test-config-base)
                           nil)

          geotiff-layers  (gf/fetch-landfire-layers geotiff-config)
          geotiff-results (gf/run-simulations
                           simulations
                           (reduce (fn [acc [layer info]] (assoc acc layer (:matrix info)))
                                   {}
                                   geotiff-layers)
                           (gf/get-envelope geotiff-config geotiff-layers)
                           (:cell-size test-config-base)
                           (gf/draw-samples rand-generator simulations (:ignition-row test-config-base))
                           (gf/draw-samples rand-generator simulations (:ignition-col test-config-base))
                           (gf/draw-samples rand-generator simulations (:max-runtime test-config-base))
                           (gf/draw-samples rand-generator simulations (:temperature test-config-base))
                           (gf/draw-samples rand-generator simulations (:relative-humidity test-config-base))
                           (gf/draw-samples rand-generator simulations (:wind-speed-20ft test-config-base))
                           (gf/draw-samples rand-generator simulations (:wind-from-direction test-config-base))
                           (gf/draw-samples rand-generator simulations (:foliar-moisture test-config-base))
                           (gf/draw-samples rand-generator simulations (:ellipse-adjustment-factor test-config-base))
                           (:outfile-suffix test-config-base)
                           (:output-geotiffs? test-config-base)
                           (:output-pngs? test-config-base)
                           (:output-csvs? test-config-base)
                           nil)]

      (is (every? some? postgis-results))

      (is (every? some? geotiff-results))

      (is (= (set postgis-results) (set geotiff-results))))))
