;; [[file:../../org/GridFire.org::*Magellan][Magellan:1]]
(ns gridfire.geotiff-bridge
  (:require [clojure.core.matrix :as m]
            [magellan.core :refer [register-new-crs-definitions-from-properties-file!
                                   make-envelope matrix-to-raster write-raster
                                   read-raster]]
            [magellan.raster.inspect :as inspect]))

(defn geotiff-raster-to-matrix
  "Reads a raster from a file using the magellan.core library. Returns the
   post-processed raster values as a Clojure matrix using the core.matrix API
   along with all of the georeferencing information associated with this tile in a
   hash-map with the following form:
  {:srid 900916,
   :upperleftx -321043.875,
   :upperlefty -1917341.5,
   :width 486,
   :height 534,
   :scalex 2000.0,
   :scaley -2000.0,
   :skewx 0.0,
   :skewy 0.0,
   :numbands 1,
   :matrix #vectorz/matrix Large matrix with shape: [534,486]}"
  [file-path]
  (let [raster   (read-raster file-path)
        grid     (:grid raster)
        r-info   (inspect/describe-raster raster)
        matrix   (first (inspect/extract-matrix raster))
        image    (:image r-info)
        envelope (:envelope r-info)]
    {:srid       (:srid r-info)
     :upperleftx (get-in envelope [:x :min])
     :upperlefty (get-in envelope [:y :min])
     :width      (:width image)
     :height     (:height image)
     :scalex     (.getScaleX (.getGridToCRS2D grid))
     :scaley     (.getScaleY (.getGridToCRS2D grid))
     :skewx      0.0 ;FIXME not used?
     :skewy      0.0 ;FIXME not used?
     :numbands   (:bands image)
     :matrix     (m/matrix matrix)}))
;; Magellan:1 ends here
