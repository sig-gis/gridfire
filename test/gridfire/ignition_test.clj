(ns gridfire.ignition-test
  (:require [clojure.test :refer [deftest testing is]]
            [gridfire.fetch :as fetch]
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
   :max-runtime               60         ;; (minutes)
   :temperature               '(50)      ;; (degrees Fahrenheit)
   :relative-humidity         '(1)       ;; (%)
   :wind-speed-20ft           '(10)      ;; (miles/hour)
   :wind-from-direction       '(0)       ;; (degrees clockwise from north)
   :foliar-moisture           90         ;; (%)
   :ellipse-adjustment-factor 1.0        ;; (< 1.0 = more circular, > 1.0 = more elliptical)
   :simulations               1
   :random-seed               1234567890 ;; long value (optional)
   :output-csvs?              true
   :fetch-layer-method        :postgis
   })

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [filename]
  (str resources-path filename))

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest fetch-ignition-layers-test
  (testing "Fetching ignition layers from postgis and geotiff files"
    (let [geotiff-config (merge test-config-base
                                {:fetch-ignition-method
                                 :geotiff

                                 :ignition-layers
                                 {:initial-fire-spread         (in-file-path "scar.tif")
                                  :initial-fire-line-intensity (in-file-path "ifi.tif")
                                  :initial-flame-length        (in-file-path "ifl.tif")}})
          postgis-config (merge test-config-base
                                {:fetch-ignition-method
                                 :postgis

                                 :ignition-layers
                                 {:initial-fire-spread         "ignition.scar WHERE rid=1"
                                  :initial-fire-line-intensity "ignition.ifi WHERE rid=1"
                                  :initial-flame-length        "ignition.ifl WHERE rid=1"}})
          geotiff-ignition-layers (fetch/initial-ignition-layers postgis-config)
          postgis-ignition-layers (fetch/initial-ignition-layers geotiff-config)]

      (is (= (get-in postgis-ignition-layers [:initial-fire-spread :matrix])
             (get-in geotiff-ignition-layers [:initial-fire-spread :matrix])))

      (is (= (get-in postgis-ignition-layers [:initial-fire-line-intensity :matrix])
             (get-in geotiff-ignition-layers [:initial-fire-line-intensity :matrix])))

      (is (= (get-in postgis-ignition-layers [:initial-flame-length :matrix])
             (get-in geotiff-ignition-layers [:initial-flame-length :matrix]))))))

(deftest omit-ignition-method-test
  (testing "Omitting fetch-igntion-method key in config"
   (let [geotiff-config          (merge test-config-base
                                        {:ignition-layers
                                         {:initial-fire-spread         (in-file-path "scar.tif")
                                          :initial-fire-line-intensity (in-file-path "ifi.tif")
                                          :initial-flame-length        (in-file-path "ifl.tif")}})
         geotiff-ignition-layers (fetch/initial-ignition-layers geotiff-config)]

     (is (nil? geotiff-ignition-layers)))))


(deftest run-simulation-test
  (testing "Running simulation with ignition layers read from geotiff files"
    (let [geotiff-config (merge test-config-base
                                {:fetch-ignition-method
                                 :geotiff

                                 :ignition-layers
                                 {:initial-fire-spread         (in-file-path "scar.tif")
                                  :initial-fire-line-intensity (in-file-path "ifi.tif")
                                  :initial-flame-length        (in-file-path "ifl.tif")}})
          simulations      (:simulations test-config-base)
          rand-generator   (if-let [seed (:random-seed test-config-base)]
                             (Random. seed)
                             (Random.))
          landfire-layers  (gf/fetch-landfire-layers geotiff-config)
          landfire-rasters (into {} (map (fn [[layer-name info]] [layer-name (:matrix info)])) landfire-layers)
          ignition-layers  (fetch/initial-ignition-layers geotiff-config)
          ignition-rasters (into {} (map (fn [[layer-name info]] [layer-name (:matrix info)])) ignition-layers)
          results          (gf/run-simulations
                            simulations
                            landfire-rasters
                            (gf/get-envelope geotiff-config landfire-layers)
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
                            ignition-rasters)]

      (is (every? some? results)))))
