(ns gridfire.spec.perturbations
  (:require [clojure.spec.alpha :as s]))

(s/def ::spatial-type #{:global :pixel})
(s/def ::pdf-min float?)
(s/def ::pdf-max float?)
(s/def ::valid-range (fn [{:keys [pdf-min pdf-max]}] (< pdf-min pdf-max)))

(s/def ::pdf-info (s/and (s/keys :req-un [::spatial-type ::pdf-min ::pdf-max])
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
