(ns gridfire.spotting-test
  (:require [gridfire.spotting :as spotting]
            [clojure.test :refer [deftest is testing]]))

(deftest deltas-axis-test
  (testing "x-axis"
    (testing "east"
      (let [H (spotting/hypotenuse 10 10)]
        (is (= [H 0] (first (spotting/deltas-wind-dir->coord [[10 10]] 45.0))))

        (is (= [H 0] (first (spotting/deltas-wind-dir->coord [[10 -10]] 135.0))))))
    (testing "west"
      (let [H (spotting/hypotenuse 10 10)]
        (is (= [(- H) 0] (first (spotting/deltas-wind-dir->coord [[10 10]] 225.0))))

        (is (= [(- H) 0] (first (spotting/deltas-wind-dir->coord [[10 -10]] 315.0)))))))
  (testing "y-axis"
    (testing "north"
      (let [H (spotting/hypotenuse 10 10)]
        (is (= [0 H] (first (spotting/deltas-wind-dir->coord [[10 -10]] 45.0))))

        (is (= [0 H] (first (spotting/deltas-wind-dir->coord [[10 10]] 315.0))))))

    (testing "south"
      (let [H (spotting/hypotenuse 10 10)]

        (is (= [0 (- H)] (first (spotting/deltas-wind-dir->coord [[10 10]] 135.0))))

        (is (= [0 (- H)] (first (spotting/deltas-wind-dir->coord [[10 -10]] 225.0))))))))
