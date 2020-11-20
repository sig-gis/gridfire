(ns gridfire.spec.output
  (:require [clojure.spec.alpha :as s]
            [gridfire.spec.common :as common]))

(defmacro one-or-more-keys [ks]
  (let [keyset (set (map (comp keyword name) ks))]
    `(s/and (s/keys :opt-un ~ks)
            #(some ~keyset (keys %)))))

(s/def ::type
  (s/or :key #{:final}
        :scalar int?))

(s/def ::fire-spread ::type)
(s/def ::flame-length ::type)
(s/def ::fire-line-intensity ::type)
(s/def ::burn-history ::type)

(s/def ::output-layers
  (common/one-or-more-keys
   [::fire-spread
    ::flame-length
    ::fire-line-intensity
    ::burn-history]))
