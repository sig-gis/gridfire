(ns gridfire.spotting-test
  (:require [gridfire.spotting :as spotting]
            [clojure.test :refer [deftest is testing]]))

(defn close-to-zero [d]
  (< (Math/abs d) 0.000001))

(deftest deltas-axis-test
  (testing "x-axis"
    (testing "east"
      (let [H (spotting/hypotenuse 10 10)]

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 10]] 45.0))]

          (is (= H dx))

          (is (close-to-zero dy)))

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 -10]] 135.0))]

          (is (= H dx))

          (is (close-to-zero dy)))))

    (testing "west"
      (let [H (spotting/hypotenuse 10 10)]

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 10]] 225.0))]

          (is (= (- H) dx))

          (is (close-to-zero dy)))

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 -10]] 315.0))]

          (is (= (- H) dx))

          (is (close-to-zero dy))))))

  (testing "y-axis"
    (testing "north"
      (let [H (spotting/hypotenuse 10 10)]

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 -10]] 45.0))]

          (is (close-to-zero dx))

          (is (= H dy)))

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 10]] 315.0))]

          (is (close-to-zero dx))

          (is (= H dy)))))

    (testing "south"
      (let [H (spotting/hypotenuse 10 10)]

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 10]] 135.0))]

          (is (close-to-zero dx))

          (is (= (- H) dy)))

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 -10]] 225.0))]

          (is (close-to-zero dx))

          (is (= (- H) dy)))))))
