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
;; Section 2: Ignition from which to build simulation inputs:4 ends here
