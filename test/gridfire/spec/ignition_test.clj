(ns gridfire.spec.ignition-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [gridfire.spec.ignition :as ignition]))

(deftest path-test
  (let [path "/some/path/to.tif"]
    (is (s/valid? ::ignition/ignition-layer path))))

(deftest sql-test
  (let [sql "landfire.ign WHERE rid=1"]
    (is (s/valid? ::ignition/ignition-layer sql))))

(deftest sql-test
  (let [sql {:path        "/some/path/to.tif"
             :burn-values {:burned   1.0
                           :unburned -1.0}}]
    (is (s/valid? ::sut/ignition-layer sql))))
