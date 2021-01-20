(ns gridfire.spec.spotting-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [gridfire.spec.spotting :as spotting]))

(deftest spotting-test
  (let [config {:ambient-gas-density         1.0
                :crown-fire-spotting-percent [0.1 0.8]
                :num-firebrands              10
                :specific-heat-gas           0.1}]
    (is (s/valid? ::spotting/spotting config))))

(deftest crown-fire-spotting-percent-test
  (testing "scalar"
    (let [config 0.1]

      (is (s/valid? ::spotting/crown-fire-spotting-percent config))))

  (testing "range"
   (let [config [0.1 0.8]]

     (is (s/valid? ::spotting/crown-fire-spotting-percent config))))

  (testing "invalid range"
    (let [config [0.8 0.1]]

      (is (not (s/valid? ::spotting/crown-fire-spotting-percent config))
          "first value is larger than the second"))))
