;; [[file:../../org/GridFire.org::*Magellan][Magellan:1]]
(ns gridfire.magellan-bridge
  (:require [clojure.core.matrix     :as m]
            [magellan.core           :refer [read-raster]]
            [magellan.raster.inspect :as inspect]))

(defn geotiff-raster-to-matrix
  "Reads a raster from a file using the magellan.core library. Returns the
   post-processed raster values as a Clojure matrix using the core.matrix API
   along with all of the georeferencing information associated with this tile in a
   hash-map with the following form:
   {:srid EPSG:32610,
    :lowerleftx -2067000.0,
    :lowerlefty 2459520.0,
    :upperleftx -2067000.0,
    :upperlefty 2460000.0,
    :width 256,
    :height 16,
    :scalex 30.0,
    :scaley -30.0,
    :skewx 0.0,
    :skewy 0.0,
    :numbands 10,
    :matrix #vectorz/matrix Large matrix with shape: [10,16,256]}"
  [file-path]
  (let [raster   (read-raster file-path)
        grid     (:grid raster)
        r-info   (inspect/describe-raster raster)
        matrix   (inspect/extract-matrix raster)
        image    (:image r-info)
        envelope (:envelope r-info)]
    {:srid       (:srid r-info)
     :lowerleftx (get-in envelope [:x :min])
     :lowerlefty (get-in envelope [:y :min])
     :upperleftx (get-in envelope [:x :min])
     :upperlefty (get-in envelope [:y :max])
     :width      (:width image)
     :height     (:height image)
     :scalex     (.getScaleX (.getGridToCRS2D grid))
     :scaley     (.getScaleY (.getGridToCRS2D grid))
     :skewx      0.0 ;FIXME not used?
     :skewy      0.0 ;FIXME not used?
     :numbands   (:bands image)
     :matrix     (m/matrix matrix)}))
;; Magellan:1 ends here
