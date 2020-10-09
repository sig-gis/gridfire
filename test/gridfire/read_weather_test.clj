(ns gridfire.read-weather-test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.core.matrix :as m]
   [gridfire.fetch :as fetch]))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def resources-path "test/gridfire/resources/")

(def db-spec {:classname   "org.postgresql.Driver"
              :subprotocol "postgresql"
              :subname     "//localhost:5432/gridfire_test"
              :user        "gridfire_test"
              :password    "gridfire_test"})

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [filename]
  (str resources-path filename))

(deftest temperature-test
  (let [config {:fetch-temperature-method :geotiff
                :temperature              (in-file-path "scar.tif")}]
    (is (m/matrix? (fetch/weather config :temperature))))

  (let [config {:fetch-temperature-method :postgis
                :temperature              "weather.temp WHERE time=1"}]))

(deftest get-weather-test
  (let [config {:db-spec                  db-spec
                :fetch-temperature-method :postgis
                :temperature              "weather.temp WHERE time=1"}]
    (is (m/matrix? (fetch/weather config :temperature)))))
