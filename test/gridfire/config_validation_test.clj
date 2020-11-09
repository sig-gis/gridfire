(ns gridfire.config-validation-test
  (:require [gridfire.spec.config :as spec]
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
        temp     (s/conform ::spec/weather
                            {:path      (in-file-path "weather-test/tmpf_to_sample.tif")
                             :cell-size low-res})
        rh       (s/conform ::spec/weather
                            {:path      (in-file-path "weather-test/rh_to_sample.tif")
                             :cell-size low-res})]
    (testing "Valid cell-size for a weather raster"
      (let [config {:cell-size   high-res
                    :temperature temp}]

        (is (true? (spec/valid-weather-cell-sizes? config)))))

    (testing "Valid cell-size for multiple weather raster"
      (let [config {:cell-size         high-res
                    :temperature       temp
                    :relative-humidity temp}]

        (is (true? (spec/valid-weather-cell-sizes? config)))))))

(deftest weather-cell-invalid-test
  (testing "Invalid cell-size for a weather raster"
    (let [cell-size (m->ft 30)
          temp      (s/conform ::spec/weather
                               {:path      (in-file-path "weather-test/tmpf_to_sample.tif")
                                :cell-size (+ cell-size (/ cell-size 2))})
          config    {:cell-size   cell-size
                     :temperature temp}]

      (is (false? (spec/valid-weather-cell-sizes? config))))))

(deftest weather-fetch-method-test
  (testing "Weather raster input (string) must have accompanying fetch method keyword"
    (let [temp   (s/conform ::spec/weather
                            (in-file-path "weather-test/tmpf_to_sample.tif"))
          config {:temperature temp}]

      (is (false? (spec/valid-weather-fetch-methods? config)))))

  (testing "Weather raster input (map) must have accompanying fetch method keyword"
    (let [temp   (s/conform ::spec/weather
                            {:path      (in-file-path "weather-test/tmpf_to_sample.tif")
                             :cell-size 80.0})
          config {:temperature temp}]

      (is (false? (spec/valid-weather-fetch-methods? config))))))
