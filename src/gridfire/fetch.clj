;; [[file:../../org/GridFire.org::*Section 2: Ignition from which to build simulation inputs][Section 2: Ignition from which to build simulation inputs:4]]
(ns gridfire.fetch
  (:require [gridfire.postgis-bridge :refer [postgis-raster-to-matrix]]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix]]
            [magellan.core :as mg]
            [magellan.raster.inspect :as inspect]))

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
          method ((keyword (interpose "-" ["fetch" stype "method"])) config)]
      (keyword (str (name method) "-" stype)))))

(defmethod weather :postgis-temperature
  [{:keys [temperature db-spec] :as config} type]
  (:matrix (postgis-raster-to-matrix db-spec temperature)))

(defmethod weather :geotiff-temperature
  [{:keys [temperature] :as config} type]
  (:matrix (geotiff-raster-to-matrix temperature)))

(defmethod weather :postgis-relative-humidity
  [{:keys [relative-humidity] :as config} type]
  (:matrix (postgis-raster-to-matrix relative-humidity)))

(defmethod weather :geotiff-relative-humidity
  [{:keys [relative-humidity] :as config} type]
  (:matrix (geotiff-raster-to-matrix relative-humidity)))

(defmethod weather :postgis-wind-speed-20ft
  [{:keys [wind-speed-20ft] :as config} type]
  (:matrix (postgis-raster-to-matrix wind-speed-20ft)))

(defmethod weather :geotiff-wind-speed-20ft
  [{:keys [wind-speed-20ft] :as config} type]
  (:matrix (geotiff-raster-to-matrix wind-speed-20ft)))

(defmethod weather :postgis-wind-from-direction
  [{:keys [wind-from-direction] :as config} type]
  (:matrix (postgis-raster-to-matrix wind-from-direction)))

(defmethod weather :geotiff-wind-from-direction
  [{:keys [wind-from-direction] :as config} type]
  (:matrix (geotiff-raster-to-matrix wind-from-direction)))

(defmethod weather :postgis-foliar-moisture
  [{:keys [foliar-moisture] :as config} type]
  (:matrix (postgis-raster-to-matrix foliar-moisture)))

(defmethod weather :geotiff-foliar-moisture
  [{:keys [foliar-moisture] :as config} type]
  (:matrix (geotiff-raster-to-matrix foliar-moisture)))

;; Section 2: Ignition from which to build simulation inputs:4 ends here
