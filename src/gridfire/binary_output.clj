(ns gridfire.binary-output
  (:import java.io.DataOutputStream)
  (:require [clojure.java.io :as io]
            [clojure.core.matrix :as m]))

;; We assume that matrix[0,0] is the upper left corner.
(defn non-zero-data [matrix]
  (let [row-count (m/row-count matrix)]
    (->> matrix
         (m/non-zero-indices)
         (reverse)
         (map-indexed (fn [row cols]
                        (let [true-row (- row-count (inc row))]
                          {:x (map inc cols)
                           :y (repeat (count cols) (inc row))
                           :value (map (fn [col] (m/mget matrix true-row col)) cols)})))
         (apply merge-with concat))))

(defn write-matrix-as-binary [matrix file-name]
  (let [num-burned-cells (m/non-zero-count matrix)
        burned-data      (non-zero-data matrix)]
    (with-open [out (DataOutputStream. (io/output-stream file-name))]
      (.writeInt out (int num-burned-cells))                          ; Int32
      (doseq [x (burned-data :x)] (.writeShort out (int x)))          ; Int16
      (doseq [y (burned-data :y)] (.writeShort out (int y)))          ; Int16
      (doseq [v (burned-data :value)] (.writeFloat out (float v)))))) ; Float32

;; toa_test.bin was created with:
;; (write-matrix-as-binary [[0 1 2] [3 0 4] [5 6 0]] "toa_test.bin")
