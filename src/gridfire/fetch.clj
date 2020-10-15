;; [[file:../../org/GridFire.org::*Section 2: Ignition from which to build simulation inputs][Section 2: Ignition from which to build simulation inputs:4]]
(ns gridfire.fetch
  (:require [clojure.string :as s]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix
                                              geotiff-raster-to-matrix-multiband]]
            [gridfire.postgis-bridge :refer [postgis-raster-to-matrix
                                             postgis-raster-to-matrix-multiband]]))

(defmulti initial-ignition-layers
  (fn [config]
    (:fetch-ignition-method config)))

(defmethod initial-ignition-layers :postgis
  [{:keys [db-spec ignition-layer] :as config}]
  (postgis-raster-to-matrix db-spec ignition-layer))

(defmethod initial-ignition-layers :geotiff
  [{:keys [ignition-layer] :as config}]
  (geotiff-raster-to-matrix ignition-layer))

(defmethod initial-ignition-layers :default
  [config]
  nil)

;;-----------------------------------------------------------------------------
;; Weather
;;-----------------------------------------------------------------------------

(defmulti weather
  (fn [config target-raster-for-resample type]
    (let [stype  (name type)
          method ((keyword (s/join "-" ["fetch" stype "method"])) config)]
      (keyword (str (name method) "-" stype)))))

(defmethod weather :postgis-temperature
  [{:keys [temperature db-spec] :as config} target-raster-for-resample type]
  (let [scalex (:scalex target-raster-for-resample)]
    (:matrix (postgis-raster-to-matrix-multiband
              db-spec
              temperature
              scalex
              nil))))

(defmethod weather :geotiff-temperature
  [{:keys [temperature] :as config} target-raster-for-resample type]
  (:matrix (geotiff-raster-to-matrix-multiband
            temperature
            (:grid target-raster-for-resample))))

(defmethod weather :postgis-relative-humidity
  [{:keys [relative-humidity db-spec] :as config} target-raster-for-resample type]
  (let [scalex (:scalex target-raster-for-resample)]
    (:matrix (postgis-raster-to-matrix-multiband
              db-spec
              relative-humidity
              scalex
              nil))))

(defmethod weather :geotiff-relative-humidity
  [{:keys [relative-humidity] :as config} target-raster-for-resample type]
  (:matrix (geotiff-raster-to-matrix-multiband
            relative-humidity
            (:grid target-raster-for-resample))))

(defmethod weather :postgis-wind-speed-20ft
  [{:keys [wind-speed-20ft db-spec] :as config} target-raster-for-resample type]
  (let [scalex (:scalex target-raster-for-resample)]
    (:matrix (postgis-raster-to-matrix-multiband
              db-spec
              wind-speed-20ft
              scalex
              nil))))

(defmethod weather :geotiff-wind-speed-20ft
  [{:keys [wind-speed-20ft] :as config} target-raster-for-resample type]
  (:matrix (geotiff-raster-to-matrix-multiband
            wind-speed-20ft
            (:grid target-raster-for-resample))))

(defmethod weather :postgis-wind-from-direction
  [{:keys [wind-from-direction db-spec] :as config} target-raster-for-resample type]
  (let [scalex (:scalex target-raster-for-resample)]
    (:matrix (postgis-raster-to-matrix-multiband
              db-spec
              wind-from-direction
              scalex
              nil))))

(defmethod weather :geotiff-wind-from-direction
  [{:keys [wind-from-direction] :as config} target-raster-for-resample type]
  (:matrix (geotiff-raster-to-matrix-multiband
            wind-from-direction
            (:grid target-raster-for-resample))))
;; Section 2: Ignition from which to build simulation inputs:4 ends here
