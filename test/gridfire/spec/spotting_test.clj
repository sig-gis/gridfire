(ns gridfire.spec.spotting-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [gridfire.spec.spotting :as spotting]))

(deftest spotting-test
  (testing "required"
   (let [config {:ambient-gas-density         1.0
                 :crown-fire-spotting-percent [0.1 0.8]
                 :num-firebrands              10
                 :specific-heat-gas           0.1}]
     (is (s/valid? ::spotting/spotting config))))

  (testing "optional surface-fire-spotting"
    (let [config {:ambient-gas-density         1.0
                  :crown-fire-spotting-percent [0.1 0.8]
                  :num-firebrands              10
                  :specific-heat-gas           0.1
                  :surface-fire-spotting       {:spotting-percent [[[1   149] 1.0]
                                                                   [[150 169] 2.0]
                                                                   [[169 204] 3.0]]}}]
      (is (s/valid? ::spotting/spotting config)))))

(deftest crown-fire-spotting-percent-test
  (testing "scalar"
    (is (s/valid? ::spotting/crown-fire-spotting-percent 0.1)))

  (testing "range"
    (is (s/valid? ::spotting/crown-fire-spotting-percent [0.1 0.8])))

  (testing "invalid range"
    (is (not (s/valid? ::spotting/crown-fire-spotting-percent [0.8 0.1]))
        "first value should not be larger than the second")))

(deftest num-firebrands-test
  (testing "scalar"
    (is (s/valid? ::spotting/num-firebrands 10)))

  (testing "range"
    (is (s/valid? ::spotting/num-firebrands {:lo 1 :hi 10})
        "lo and hi can both be scalar")

    (is (s/valid? ::spotting/num-firebrands {:lo [1 2] :hi [9 10]})
        "lo and hi can both be ranges")

    (is (s/valid? ::spotting/num-firebrands {:lo 1 :hi [9 10]})
        "lo and hi can either be scalar or range"))

  (testing "edge cases"
    (is (s/valid? ::spotting/num-firebrands {:lo 1 :hi [1 2]})
        "edge of ranges can overlap")

    (is (s/valid? ::spotting/num-firebrands {:lo [1 2] :hi [2 3]})
        "edge of ranges can overlap"))

  (testing "invalid range"
    (let [config {:lo 10
                  :hi 1 }]
      (is (not (s/valid? ::spotting/num-firebrands config))
          "lo syhould not be higher than hi"))

    (let [config {:lo [1 3]
                  :hi [2 3]}]
      (is (not (s/valid? ::spotting/num-firebrands config))
          "should not have overlapping ranges"))))

(deftest surface-fire-spotting-test
  (testing "scalar percents"
    (let [config [[[1   149] 1.0]
                  [[150 169] 2.0]
                  [[169 204] 3.0]]]

      (is (s/valid? ::spotting/surface-fire-spotting-percent config))))

  (testing "range percents"
    (let [config [[[1   149] [1.0 2.0]]
                  [[150 169] [3.0 4.0]]
                  [[169 204] [0.2 0.4]]]]

      (is (s/valid? ::spotting/surface-fire-spotting-percent config)))))
