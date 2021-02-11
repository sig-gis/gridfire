(ns gridfire.binary-output
  (:require [clojure.core.matrix :as m]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [gridfire.conversion :as convert])
  (:import [java.io DataInputStream DataOutputStream]))

;; We assume that matrix[0,0] is the upper left corner.
(defn non-zero-data [matrix]
  (let [row-count (m/row-count matrix)]
    (->> matrix
         (m/non-zero-indices)
         (reverse)
         (map-indexed (fn [row cols]
                        (let [true-row (- row-count (inc row))]
                          {:x     (map inc cols)
                           :y     (repeat (count cols) (inc row))
                           :value (map (fn [col] (m/mget matrix true-row col)) cols)})))
         (apply merge-with concat))))

(defn indices-to-matrix
  ([indices]
   (indices-to-matrix indices nil))

  ([indices ttype]
   (let [rows    (indices :y)
         cols    (indices :x)
         values  (indices :value)
         max-row (apply max rows)
         max-col (apply max cols)
         matrix  (if (= ttype :int)
                   (make-array Integer/TYPE max-row max-col)
                   (make-array Float/TYPE max-row max-col))]
     (dotimes [i (count values)]
       (let [true-row (- max-row (rows i))
             true-col (dec (cols i))]
         (aset matrix true-row true-col (values i))))
     (m/matrix matrix))))

(defn write-matrix-as-binary [matrix file-name]
  (let [num-burned-cells (m/non-zero-count matrix)
        burned-data      (non-zero-data matrix)]
    (with-open [out (DataOutputStream. (io/output-stream file-name))]
      (.writeInt out (int num-burned-cells))                          ; Int32
      (doseq [x (burned-data :x)] (.writeInt out (int x)))            ; Int32
      (doseq [y (burned-data :y)] (.writeInt out (int y)))            ; Int32
      (doseq [v (burned-data :value)] (.writeFloat out (float v)))))) ; Float32

(defn- write-val [out type v]
  (case type
    :float (.writeFloat out (float v))
    :int   (.writeInt out (int v))
    nil))

(defn write-matrices-as-binary
  [matrices file-name]
  (let [num-burned-cells (m/non-zero-count (:matrix (first matrices)))
        data             (map (fn [{:keys [ttype matrix]}]
                                {:ttype ttype
                                 :data  (non-zero-data matrix)})
                              matrices)]
    (with-open [out (DataOutputStream. (io/output-stream file-name))]
      (.writeInt out (int num-burned-cells))                                  ; Int32
      (doseq [x (get-in (first data) [:data :x])] (.writeInt out (int x)))    ; Int32
      (doseq [y (get-in (first data) [:data :y])] (.writeInt out (int y)))    ; Int32
      (doseq [d data
              v (get-in d [:data :value])]
        (write-val out (:ttype d) v)))))

(defn read-matrix-as-binary [file-name]
  (with-open [in (DataInputStream. (io/input-stream file-name))]
    (let [num-burned-cells (.readInt in)]                                ; Int32
      (indices-to-matrix
       {:x     (vec (repeatedly num-burned-cells #(.readInt in)))        ; Int32
        :y     (vec (repeatedly num-burned-cells #(.readInt in)))        ; Int32
        :value (vec (repeatedly num-burned-cells #(.readFloat in)))})))) ; Float32

(defn- read-val [in ttype]
  (case ttype
    :float (.readFloat in)
    :int   (.readInt in)
    nil))

(defn read-matrices-as-binary [file-name ttypes]
  (with-open [in (DataInputStream. (io/input-stream file-name))]
    (let [num-burned-cells (.readInt in)
          xs               (vec (repeatedly num-burned-cells #(.readInt in)))  ; Int32
          ys               (vec (repeatedly num-burned-cells #(.readInt in)))] ; Int32
      (mapv (fn [ttype]
              (indices-to-matrix
               {:x     xs
                :y     ys
                :value (vec (repeatedly num-burned-cells #(read-val in ttype)))} ; Float32/Int32
               ttype))
            ttypes))))

(defn write-two-int-file [file-name]
  (with-open [out (DataOutputStream. (io/output-stream file-name))]
    (.writeInt out 1)   ; Int32
    (.writeInt out 2))) ; Int32

;;-----------------------------------------------------------------------------
;; Post Process file
;;-----------------------------------------------------------------------------

(defn build-post-process-config
  [{:keys [cell-size simulations output-binary]} raster csv-filename]
  (array-map
   "NX"                       (:width raster)
   "NY"                       (:height raster)
   "NCASES"                   simulations
   "XLLCORNER"                (:lowerleftx raster)
   "YLLCORNER"                (:lowerlefty raster)
   "CELLSIZE"                 (convert/ft->m cell-size)
   "NUM_TIMESTEPS"            (:num-timesteps output-binary)
   "DT"                       (:dt output-binary)
   "OUTPUTS_DIRECTORY"        "'/gridfire/output'"
   "FIRE_SIZE_STATS_FILENAME" (str "'" csv-filename "'")
   "POSTPROCESS_TYPE"         1
   "BINARY_FILE_TYPE"         2
   "PATH_TO_GDAL"             "'/usr/bin/gdalinfo'"
   "SCRATCH"                  "'scratch'"))

(defn process-inputs [config]
  (map (fn [[k v]]
         (str (s/join "=" [k (str v)]) "\n"))
       config))

(defn write-bin-to-geotiff-config-file!
  "Writes a configuration file for post processing binary files into geotiffs."
  [file-name {:keys [output-directory] :as config} raster csv-filename]
  (with-open [out (io/writer (if output-directory
                               (s/join "/" [output-directory file-name])
                               file-name))]
    (.write out (str "&ELMFIRE_POST_INPUTS\n"))
    (let [entries (-> (build-post-process-config config raster csv-filename)
                      process-inputs)]
      (doseq [entry entries]
        (.write out entry)))))
