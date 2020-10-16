(ns gridfire.fetch-ignition-test
  (:require [clojure.test :refer [deftest is testing]]
            [gridfire.fetch :as fetch]))

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
    :fetch-layer-method        :postgis })

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
    (let [geotiff-config          (merge test-config-base
                                         {:fetch-ignition-method :geotiff
                                          :ignition-layer        (in-file-path "ign.tif")})
          postgis-config          (merge test-config-base
                                         {:fetch-ignition-method :postgis
                                          :ignition-layer        "ignition.ign WHERE rid=1"})
          geotiff-ignition-layers (fetch/initial-ignition-layers postgis-config)
          postgis-ignition-layers (fetch/initial-ignition-layers geotiff-config)]

      (is (= (:matrix geotiff-ignition-layers)
             (:matrix postgis-ignition-layers))))))

(deftest omit-ignition-method-test
  (testing "Omitting fetch-igntion-method key in config"
    (let [geotiff-config          (merge test-config-base
                                         {:ignition-layer (in-file-path "ign.tif")})
          geotiff-ignition-layers (fetch/initial-ignition-layers geotiff-config)]

      (is (nil? geotiff-ignition-layers)))))
