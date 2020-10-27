(ns gridfire.config-validation-test
  (:require [gridfire.validation :as validation]
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
  (testing "Valid cell-size for a weather raster"
    (let [config       {:cell-size   800
                        :temperature {:path      (in-file-path "weather-test/tmpf_to_sample.tif")
                                      :cell-size 80}}
          [result err] (validation/weather-cell-size config)]

      (is (= config result))

      (is (nil? err))))

  (testing "Valid cell-size for multiple weather raster"
    (let [config       {:cell-size         800
                        :temperature       {:path      (in-file-path "weather-test/tmpf_to_sample.tif")
                                            :cell-size 80}
                        :relative-humidity {:path      (in-file-path "weather-test/rh_to_sample.tif")
                                            :cell-size 80}}
          [result err] (validation/weather-cell-size config)]

      (is (= config result))

      (is (nil? err))))

  (testing "Invalid cell-size for a weather raster"
    (let [config       {:cell-size   800
                        :temperature {:path      (in-file-path "weather-test/tmpf_to_sample.tif")
                                      :cell-size 81}}
          [result err] (validation/weather-cell-size config)]

      (is (nil? result))

      (is (some? err)))))

(deftest weather-fetch-method-test
  (testing "Weather raster input (string) must have accompanying fetch method keyword"
    (let [config       {:temperature (in-file-path "weather-test/tmpf_to_sample.tif")}
          [result err] (validation/weather-fetch-methods config)]

      (is (nil? result))

      (is (some? err))

      (is (= err "Missing these fetch-<weather>-method keywords: (:fetch-temperature-method)"))))

  (testing "Weather raster input (map) must have accompanying fetch method keyword"
    (let [config       {:temperature {:path      (in-file-path "weather-test/tmpf_to_sample.tif")
                                      :cell-size 80}}
          [result err] (validation/weather-fetch-methods config)]

      (is (nil? result))

      (is (some? err))

      (is (= err "Missing these fetch-<weather>-method keywords: (:fetch-temperature-method)"))))
  )
