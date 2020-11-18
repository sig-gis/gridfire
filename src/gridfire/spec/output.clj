(ns gridfire.spec.output
  (:require [clojure.spec.alpha :as s]
            [gridfire.spec.common :as common]))

(defmacro one-or-more-keys [ks]
  (let [keyset (set (map (comp keyword name) ks))]
    `(s/and (s/keys :opt-un ~ks)
            #(some ~keyset (keys %)))))

(s/def ::output
  (s/or :key #{:final}
        :scalar int?
        :vector (s/coll-of int? :kind vector? :count 2)
        :list (s/coll-of int? :kind list?)))

(s/def ::fire-spread ::output)
(s/def ::flame-length ::output)
(s/def ::fire-line-intensity ::output)
(s/def ::burn-time ::output)

(s/def ::output-layers
  (common/one-or-more-keys
   [::fire-spread
    ::flame-length
    ::fire-line-intensity
    ::burn-time]))

(s/valid? ::output-layers {:fire-spread :final})
(s/valid? ::output-layers {:fire-spread 0})
