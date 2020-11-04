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
  (fn [config type]
    (let [stype  (name type)
          method ((keyword (s/join "-" ["fetch" stype "method"])) config)]
      (keyword (str (name method) "-" stype)))))

(defmethod weather :postgis-temperature
  [{:keys [temperature db-spec] :as config} type]
  (let [sql (if (map? temperature) (:sql temperature) temperature)]
    (:matrix (postgis-raster-to-matrix-multiband db-spec sql))))

(defmethod weather :geotiff-temperature
  [{:keys [temperature] :as config} type]
  (let [path (if (map? temperature) (:path temperature) temperature)]
   (:matrix (geotiff-raster-to-matrix-multiband path))))

(defmethod weather :postgis-relative-humidity
  [{:keys [relative-humidity db-spec] :as config} type]
  (let [sql (if (map? relative-humidity) (:sql relative-humidity) relative-humidity)]
   (:matrix (postgis-raster-to-matrix-multiband db-spec sql))))

(defmethod weather :geotiff-relative-humidity
  [{:keys [relative-humidity] :as config} type]
  (let [path (if (map? relative-humidity) (:path relative-humidity) relative-humidity)]
   (:matrix (geotiff-raster-to-matrix-multiband path))))

(defmethod weather :postgis-wind-speed-20ft
  [{:keys [wind-speed-20ft db-spec] :as config} type]
  (let [sql (if (map? wind-speed-20ft) (:sql wind-speed-20ft) wind-speed-20ft)]
   (:matrix (postgis-raster-to-matrix-multiband db-spec sql))))

(defmethod weather :geotiff-wind-speed-20ft
  [{:keys [wind-speed-20ft] :as config} type]
  (let [path (if (map? wind-speed-20ft) (:path wind-speed-20ft) wind-speed-20ft)]
      (:matrix (geotiff-raster-to-matrix-multiband path))))

(defmethod weather :postgis-wind-from-direction
  [{:keys [wind-from-direction db-spec] :as config} type]
  (let [sql (if (map? wind-from-direction) (:sql wind-from-direction) wind-from-direction)]
      (:matrix (postgis-raster-to-matrix-multiband db-spec sql))))

(defmethod weather :geotiff-wind-from-direction
  [{:keys [wind-from-direction] :as config} type]
  (let [path (if (map? wind-from-direction) (:path wind-from-direction) wind-from-direction)]
    (:matrix (geotiff-raster-to-matrix-multiband path))))
;; Section 2: Ignition from which to build simulation inputs:4 ends here
