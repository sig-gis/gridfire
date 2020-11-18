(ns gridfire.spec.ignition-test
  (:require [gridfire.spec.ignition :as sut]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.test :refer [deftest is testing]]))

(deftest path-test
  (let [path "/some/path/to.tif"]
    (is (s/valid? ::sut/ignition-layer path))))

(deftest sql-test
  (let [sql "landfire.ign WHERE rid=1"]
    (is (s/valid? ::sut/ignition-layer sql))))

(deftest sql-test
  (let [sql {:path "/some/path/to.tif"
             :burn-values {:burned 1.0
                           :unburned -1.0}}]
    (is (s/valid? ::sut/ignition-layer sql))))
