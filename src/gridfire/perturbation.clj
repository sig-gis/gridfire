;; [[file:../../org/GridFire.org::perturbation][perturbation]]
(ns gridfire.perturbation
  (:require [gridfire.utils.random :refer [random-float]]))

(defn- enrich-info
  [perturbations rand-generator id]
  (reduce-kv
   (fn [acc k {:keys [spatial-type range rand-gen] :as v}]
     (let [simulation-id     id
           [min-val max-val] range]
       (if (= spatial-type :global)
         (update-in acc [k] merge {:global-value   (random-float min-val max-val rand-generator)
                                   :rand-generator rand-generator})
         (update-in acc [k] merge {:simulation-id  simulation-id
                                   :rand-generator rand-generator}))))
   perturbations
   perturbations))

(defn draw-samples
  [rand-generator n perturbations]
  (when perturbations
    (mapv #(enrich-info perturbations rand-generator %) (range n))))

(defn value-at
  ([perturb-info matrix here]
   (value-at perturb-info matrix here nil))

  ([{:keys [range spatial-type global-value rand-generator]} matrix here frequency-band]
   (let [[min-val max-val] range]
     (if (= spatial-type :global)
       global-value
       (random-float min-val max-val rand-generator)))))

(def value-at
  (memoize value-at))

(defn- update?
  [global-clock next-clock frequency]
  (< (quot global-clock frequency)
     (quot next-clock frequency)))

(defn- global-temporal-perturbations
  [perturbations]
  (->> perturbations
       (filter (fn [[k v]] (and (:frequency v) (= (:spatial-type v) :global))))
       keys))

(defn update-global-vals
  [{:keys [perturbations] :as constants} current-clock next-clock]
  (let [layers-to-update (global-temporal-perturbations perturbations)]
    (reduce
     (fn [acc layer-name]
       (let [{:keys [frequency
                     range
                     rand-generator]} (get-in acc [:perturbations layer-name])
             [min-val max-val]        range
             new-global               (random-float min-val max-val rand-generator)]
         (if (update? current-clock next-clock frequency)
           (assoc-in acc [:perturbations layer-name :global-value] new-global)
           acc)))
     constants
     layers-to-update)))
;; perturbation ends here
