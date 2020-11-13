(ns gridfire.perturbation-test
  (:require [clojure.test :refer [deftest is testing]]
            [gridfire.perturbation :as perturbation])
  (:import java.util.Random))

(deftest enrich-info-test
  (testing "global spatial type"
    (let [perturbations {:canopy-cover       {:spatial-type :global
                                              :range        [-1 1]}
                         :canopy-base-height {:spatial-type :global
                                              :range        [-1 1]}}
          p-info        (#'perturbation/enrich-info perturbations (Random.) 1)]

      (is (every? (fn [[k v]] (contains? v :global-value)) p-info))

      (is (every? (fn [[k v]] (contains? v :rand-generator)) p-info))))

  (testing "pixel spatial type"
    (let [perturbations {:canopy-cover       {:spatial-type :pixel
                                              :range        [-1 1]}
                         :canopy-base-height {:spatial-type :pixel
                                              :range        [-1 1]}}
          p-info        (#'perturbation/enrich-info perturbations (Random.) 1)]

      (is (every? (fn [[k v]] (contains? v :simulation-id)) p-info))

      (is (every? (fn [[k v]] (contains? v :rand-generator)) p-info)))))

(deftest value-at-test
  (testing "memoization of value-at"
    (let [perturb-info {:rand-generator (Random.)
                        :spatial-type   :pixel
                        :range          [-1 1]}
          matrix       [[0 0] [0 0]]]
      (testing "different cells"
        (let [v1 (perturbation/value-at perturb-info matrix [0 0])
              v2 (perturbation/value-at perturb-info matrix [0 0])
              v3 (perturbation/value-at perturb-info matrix [0 1])]

          (is (= v1 v2)
              "should have the same pertubation if accessing the same cell")

          (is (not= v1 v3)
              "should have a different perturbation if accessing a different cell")))

      (testing "different frequency bands"
        (let [v1 (perturbation/value-at perturb-info matrix [0 0] 0)
              v2 (perturbation/value-at perturb-info matrix [0 0] 0)
              v3 (perturbation/value-at perturb-info matrix [0 0] 1)]

          (is (= v1 v2)
              "should have the same pertubation if accessing the same cell in the same band")

          (is (not= v1 v3)
              "should have a different perturbation if accessing same cell in different band"))))))

(deftest update-global-vals-test
  (let [old-value     3.0
        constants     {:perturbations {:canopy-cover {:spatial-type   :global
                                                      :range          [-1 1]
                                                      :global-value   old-value
                                                      :frequency      30
                                                      :rand-generator (Random.)}}}]
    (testing "different frequency band"
      (let [current-clock 0.0
            next-clock    30.0
            new-constants (perturbation/update-global-vals constants current-clock next-clock)]
       (is (not= (get-in new-constants [:perturbations :canopy-cover :global-value])
                 old-value)
           "should update map with new global peturbation value")))

    (testing "same frequency band"
      (let [current-clock 0.0
            next-clock    20.0
            new-constants (perturbation/update-global-vals constants current-clock next-clock)]
        (is (= (get-in new-constants [:perturbations :canopy-cover :global-value])
                  old-value)
            "should not update map with new global peturbation value")))

    (testing "next time step crosses multiple frequency band"
      (let [current-clock 0.0
            next-clock    300.0
            new-constants (perturbation/update-global-vals constants current-clock next-clock)]
        (is (not= (get-in new-constants [:perturbations :canopy-cover :global-value])
               old-value)
            "should update map with new global peturbation value")))))
