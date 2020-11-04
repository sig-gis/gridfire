(ns gridfire.config-validation-test
  (:require [gridfire.validation :as validation]
            [gridfire.crown-fire :refer [m->ft]]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def resources-path "test/gridfire/resources/")

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [filename]
  (str resources-path filename))

;;-----------------------------------------------------------------------------
;; Validator tests
;;-----------------------------------------------------------------------------

(deftest weather-cell-size-test
  (let [high-res (m->ft 30)
        low-res  (* high-res 10)
        temp     (s/conform ::validation/weather
                            {:path      (in-file-path "weather-test/tmpf_to_sample.tif")
                             :cell-size low-res})
        rh       (s/conform ::validation/weather
                            {:path      (in-file-path "weather-test/rh_to_sample.tif")
                             :cell-size low-res})]
    (testing "Valid cell-size for a weather raster"
      (let [config {:cell-size   high-res
                    :temperature temp}]

        (is (true? (validation/valid-weather-cell-sizes? config)))))

    (testing "Valid cell-size for multiple weather raster"
      (let [config {:cell-size         high-res
                    :temperature       temp
                    :relative-humidity temp}]

        (is (true? (validation/valid-weather-cell-sizes? config)))))))

(deftest weather-cell-invalid-test
  (testing "Invalid cell-size for a weather raster"
    (let [cell-size (m->ft 30)
          temp      (s/conform ::validation/weather
                               {:path      (in-file-path "weather-test/tmpf_to_sample.tif")
                                :cell-size (+ cell-size (/ cell-size 2))})
          config    {:cell-size   cell-size
                     :temperature temp}]

      (is (false? (validation/valid-weather-cell-sizes? config))))))

(deftest weather-fetch-method-test
  (testing "Weather raster input (string) must have accompanying fetch method keyword"
    (let [temp   (s/conform ::validation/weather
                            (in-file-path "weather-test/tmpf_to_sample.tif"))
          config {:temperature temp}]

      (is (false? (validation/valid-weather-fetch-methods? config)))))

  (testing "Weather raster input (map) must have accompanying fetch method keyword"
    (let [temp   (s/conform ::validation/weather
                            {:path      (in-file-path "weather-test/tmpf_to_sample.tif")
                             :cell-size 80.0})
          config {:temperature temp}]

      (is (false? (validation/valid-weather-fetch-methods? config))))))
