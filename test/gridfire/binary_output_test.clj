(ns gridfire.binary-output-test
  (:require [clojure.core.matrix :as m]
            [clojure.test :refer [deftest is use-fixtures]]
            [gridfire.utils.test :as utils]
            [gridfire.binary-output :as binary]))

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(use-fixtures :once utils/with-temp-output-dir)

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(def binary-file (utils/out-file-path "toa_test.bin"))

(deftest write-matrix-as-binary-test
  (let [matrix [[0.0 1.0 2.0] [3.0 0.0 4.0] [5.0 6.0 0.0]]]
    (binary/write-matrix-as-binary matrix binary-file)
    (is (= (m/matrix matrix) (binary/read-matrix-as-binary binary-file)))))

(deftest write-matrices-as-binary-test
  (let [matrices {:burn-time-matrix    [[0.0 1.0 2.0] [3.0 0.0 4.0] [5.0 6.0 0.0]]
                  :flame-length-matrix [[0.0 1.0 1.0] [1.0 0.0 1.0] [1.0 1.0 0.0]]
                  :spread-rate-matrix  [[0.0 2.0 2.0] [2.0 0.0 2.0] [2.0 2.0 0.0]]
                  :fire-type-matrix    [[0.0 3.0 3.0] [3.0 0.0 3.0] [3.0 3.0 0.0]]}
        _        (binary/write-matrices-as-binary (vals matrices) binary-file)
        result   (binary/read-matrices-as-binary binary-file (count (keys matrices)))]
    (is (= (m/matrix (:burn-time-matrix matrices)) (first result)))

    (is (= (m/matrix (:flame-length-matrix matrices)) (second result)))

    (is (= (m/matrix (:spread-rate-matrix matrices)) (nth result 2)))

    (is (= (m/matrix (:fire-type-matrix matrices)) (nth result 3)))))
