(ns gridfire.config-test
  (:require [gridfire.config :as config]
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
;; Tests
;;-----------------------------------------------------------------------------

(deftest convert
  (is (= 42 (config/convert "42")))

  (is (= 42.0 (config/convert "42.0")))

  (is (= 42.0 (config/convert "42.")))

  (is (= -42.0 (config/convert "-42.0")))

  (is (= true (config/convert ".TRUE.")))

  (is (= false (config/convert ".FALSE.")))

  (is (= "some/directory" (config/convert "'some/directory'"))))

(deftest read-data-test
  (let [x (config/-main (in-file-path "sample.data")
                        (in-file-path "sample.ctl"))]

    (clojure.pprint/pprint x)))
