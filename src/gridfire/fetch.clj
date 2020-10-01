(ns gridfire.fetch
  (:require [gridfire.postgis-bridge :refer [postgis-raster-to-matrix]]
            [gridfire.geotiff-bridge :refer [geotiff-raster-to-matrix]]
            [magellan.core :as mg]
            [magellan.raster.inspect :as inspect]))

(def ignition-names
  [:initial-fire-spread
   :initial-flame-length
   :initial-fire-line-intensity])

(defmulti initial-ignition-layers
  "Returns a map of ignition rasters (represented as maps):
  {:initial-fire-spread         raster-map
   :initial-flame-length        raster-map
   :initial-fire-line-intensity raster-map}"
  (fn [config]
    (:fetch-ignition-method config)))

(defmethod initial-ignition-layers :postgis
  [{:keys [db-spec ignition-layers] :as config}]
  (reduce
   (fn [amap ignition-name]
     (let [table  (ignition-layers ignition-name)
           raster (postgis-raster-to-matrix db-spec table)]
       (assoc amap ignition-name raster)))
   {}
   ignition-names))

(defmethod initial-ignition-layers :geotiff
  [{:keys [ignition-layers] :as config}]
  (reduce
   (fn [amap ignition-name]
     (let [geotiff (ignition-layers ignition-name)
           raster  (geotiff-raster-to-matrix geotiff)]
       (assoc amap ignition-name raster)))
   {}
   ignition-names))

(defmethod initial-ignition-layers :default
  [config]
  nil)
