;; [[file:../../org/GridFire.org::postgis-bridge][postgis-bridge]]
(ns gridfire.postgis-bridge
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.core.matrix :as m])
  (:import (org.postgresql.jdbc PgArray)))

(m/set-current-implementation :vectorz)

(defn postgis-raster-to-matrix
  "Send a SQL query to the PostGIS database given by db-spec for a
  raster tile from table table-name. Optionally resample the raster to
  match resolution and set any values below threshold to 0. Return the
  post-processed raster values as a Clojure matrix using the
  core.matrix API along with all of the georeferencing information
  associated with this tile in a hash-map with the following form:
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
  ([db-spec table-name]
   (let [meta-query (str "SELECT (ST_Metadata(rast)).* FROM " table-name)
         data-query (str "SELECT ST_DumpValues(rast,1) AS matrix FROM " table-name)]
     (jdbc/with-db-transaction [conn db-spec]
       (let [metadata (first (jdbc/query conn [meta-query]))
             matrix   (when-let [result (seq (jdbc/query conn [data-query]))]
                        (->> (first result)
                             :matrix
                             (#(.getArray ^PgArray %))
                             (m/emap #(or % -1.0))
                             m/matrix))]
         (assoc metadata :matrix matrix)))))
  ([db-spec table-name resolution threshold]
   (let [rescale-query   (if resolution
                           (format "ST_Rescale(rast,%s,-%s,'NearestNeighbor')"
                                   resolution resolution)
                           "rast")
         threshold-query (if threshold
                           (format (str "ST_MapAlgebra(%s,NULL,"
                                        "'CASE WHEN [rast.val] < %s"
                                        " THEN 0.0 ELSE [rast.val] END')")
                                   rescale-query threshold)
                           rescale-query)
         meta-query      (format "SELECT (ST_Metadata(%s)).* FROM %s"
                                 threshold-query table-name)
         data-query      (format "SELECT ST_DumpValues(%s,1) AS matrix FROM %s"
                                 threshold-query table-name)]
     (jdbc/with-db-transaction [conn db-spec]
       (let [metadata (first (jdbc/query conn [meta-query]))
             matrix   (when-let [result (seq (jdbc/query conn [data-query]))]
                        (->> (first result)
                             :matrix
                             (#(.getArray ^PgArray %))
                             (m/emap #(or % -1.0))
                             m/matrix))]
         (assoc metadata :matrix matrix))))))

(defn extract-matrices [result]
  (->> result
       :matrix
       .getValue
       read-string
       second
       (clojure.string/replace #"\{" "[")
       (clojure.string/replace #"\}" "]")
       read-string
       (m/emap #(or % -1.0))
       m/matrix))

(defn postgis-raster-to-matrix-multiband
  "Send a SQL query to the PostGIS database given by db-spec for a
  raster tile from table table-name. Optionally resample the raster to
  match resolution and set any values below threshold to 0. Return the
  post-processed raster values as a Clojure matrix using the
  core.matrix API along with all of the georeferencing information
  associated with this tile in a hash-map with the following form:
  {:srid 900916,
   :upperleftx -321043.875,
   :upperlefty -1917341.5,
   :width 486,
   :height 534,
   :scalex 2000.0,
   :scaley -2000.0,
   :skewx 0.0,
   :skewy 0.0,
   :numbands 10,
   :matrix #vectorz/matrix Large matrix with shape: [10, 534,486]}"
  ([db-spec table-name]
   (let [meta-query (str "SELECT (ST_Metadata(rast)).* FROM " table-name)
         data-query (str "SELECT ST_DumpValues(rast) AS matrix FROM " table-name)]
     (jdbc/with-db-transaction [conn db-spec]
       (let [metadata (first (jdbc/query conn [meta-query]))
             matrix   (when-let [results (seq (jdbc/query conn [data-query]))]
                        (m/matrix (mapv extract-matrices results)))]
         (assoc metadata :matrix matrix)))))

  ([db-spec table-name resolution threshold]
   (let [rescale-query   (if resolution
                           (format "ST_Rescale(rast,%s,-%s,'NearestNeighbor')"
                                   resolution resolution)
                           "rast")
         threshold-query (if threshold
                           (format (str "ST_MapAlgebra(%s,NULL,"
                                        "'CASE WHEN [rast.val] < %s"
                                        " THEN 0.0 ELSE [rast.val] END')")
                                   rescale-query threshold)
                           rescale-query)
         meta-query      (format "SELECT (ST_Metadata(%s)).* FROM %s"
                                 threshold-query table-name)
         data-query      (format "SELECT ST_DumpValues(%s,1) AS matrix FROM %s"
                                 threshold-query table-name)]
     (jdbc/with-db-transaction [conn db-spec]
       (let [metadata (first (jdbc/query conn [meta-query]))
             matrix   (when-let [results (seq (jdbc/query conn [data-query]))]
                        (m/matrix (mapv extract-matrices results)))]
         (assoc metadata :matrix matrix))))))
;; postgis-bridge ends here
