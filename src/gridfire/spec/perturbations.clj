(ns gridfire.spec.perturbations
  (:require [clojure.spec.alpha :as s]))

(s/def ::spatial-type #{:global :pixel})

(s/def ::valid-range (fn [{:keys [range]}]
                       (let [[min-val max-val] range]
                         (< min-val max-val))))

(s/def ::range (s/coll-of float? :kind vector? :count 2))

(s/def ::frequency int?)

(s/def ::pdf-info (s/and (s/keys :req-un [::spatial-type ::range]
                                 :opt-un [::frequency])
                         ::valid-range))

(s/def ::canopy-height ::pdf-info)
(s/def ::canopy-base-height ::pdf-info)
(s/def ::crown-bulk-density ::pdf-info)
(s/def ::canopy-cover ::pdf-info)
(s/def ::fuel-model ::pdf-info)

(defmacro one-or-more-keys [ks]
  (let [keyset (set (map (comp keyword name) ks))]
    `(s/and (s/keys :opt-un ~ks)
            #(some ~keyset (keys %)))))

(s/def ::perturbations
  (one-or-more-keys [::canopy-height
                     ::canopy-base-height
                     ::crown-bulk-density
                     ::canopy-cover
                     ::fuel-model]))
