(ns gridfire.ignition-test
  (:require [clojure.test :refer [deftest testing is]]
            [gridfire.fetch :as fetch]
            [gridfire.cli :as gf]))

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
   :landfire-layers           {:aspect             "landfire.asp WHERE rid=100"
                               :canopy-base-height "landfire.cbh WHERE rid=100"
                               :canopy-cover       "landfire.cc WHERE rid=100"
                               :canopy-height      "landfire.ch WHERE rid=100"
                               :crown-bulk-density "landfire.cbd WHERE rid=100"
                               :fuel-model         "landfire.fbfm40 WHERE rid=100"
                               :slope              "landfire.slp WHERE rid=100"
                               :elevation          "landfire.dem WHERE rid=100"}
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

(deftest ignition-rasters-geotiff-test
  (testing "Running Siulation with ignition rasters via geotiff files"
    (let [geotiff-config (merge
                          test-config-base
                          {:fetch-ignition-method
                           :geotiff

                           :ignition-layers
                           {:initial-fire-spread         (in-file-path "scar.tif")
                            :initial-fire-line-intensity (in-file-path "ifi.tif")
                            :initial-flame-length        (in-file-path "ifl.tif")}})
          postgis-config (merge
                          test-config-base
                          {:fetch-ignition-method
                           :postgis

                           :ignition-layers
                           {:initial-fire-spread         "ignition.scar WHERE rid=1"
                            :initial-fire-line-intensity "ignition.ifi WHERE rid=1"
                            :initial-flame-length        "ignition.ifl WHERE rid=1"}})
          postgis-ignition-layers (fetch/initial-ignition-layers geotiff-config)
          geotiff-ignition-layers (fetch/initial-ignition-layers postgis-config)]

      (is (= (get-in postgis-ignition-layers [:initial-fire-spread :matrix])
             (get-in geotiff-ignition-layers [:initial-fire-spread :matrix])))

      (is (= (get-in postgis-ignition-layers [:initial-fire-line-intensity :matrix])
             (get-in geotiff-ignition-layers [:initial-fire-line-intensity :matrix])))

      (is (= (get-in postgis-ignition-layers [:initial-flame-length :matrix])
               (get-in geotiff-ignition-layers [:initial-flame-length :matrix]))))))
