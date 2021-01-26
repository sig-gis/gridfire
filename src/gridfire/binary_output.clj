(ns gridfire.binary-output
  (:import (java.io DataInputStream DataOutputStream))
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

(defn indices-to-matrix [indices]
  (let [rows    (indices :y)
        cols    (indices :x)
        values  (indices :value)
        max-row (apply max rows)
        max-col (apply max cols)
        matrix  (make-array Float/TYPE max-row max-col)]
    (dotimes [i (count values)]
      (let [true-row (- max-row (rows i))
            true-col (dec (cols i))]
        (aset matrix true-row true-col (values i))))
    (m/matrix matrix)))

(defn write-matrix-as-binary [matrix file-name]
  (let [num-burned-cells (m/non-zero-count matrix)
        burned-data      (non-zero-data matrix)]
    (with-open [out (DataOutputStream. (io/output-stream file-name))]
      (.writeInt out (int num-burned-cells))                          ; Int32
      (doseq [x (burned-data :x)] (.writeInt out (int x)))            ; Int32
      (doseq [y (burned-data :y)] (.writeInt out (int y)))            ; Int32
      (doseq [v (burned-data :value)] (.writeFloat out (float v)))))) ; Float32

(defn read-matrix-as-binary [file-name]
  (with-open [in (DataInputStream. (io/input-stream file-name))]
    (let [num-burned-cells (.readInt in)]                                ; Int32
      (indices-to-matrix
       {:x     (vec (repeatedly num-burned-cells #(.readInt in)))        ; Int32
        :y     (vec (repeatedly num-burned-cells #(.readInt in)))        ; Int32
        :value (vec (repeatedly num-burned-cells #(.readFloat in)))})))) ; Float32

(defn write-two-int-file [file-name]
  (with-open [out (DataOutputStream. (io/output-stream file-name))]
    (.writeInt out 1)   ; Int32
    (.writeInt out 2))) ; Int32

;; toa_test.bin was created with:
;; (write-matrix-as-binary [[0 1 2] [3 0 4] [5 6 0]] "toa_test.bin")
