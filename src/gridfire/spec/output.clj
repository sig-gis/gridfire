(ns gridfire.spec.output
  (:require [clojure.spec.alpha :as s]
            [gridfire.spec.common :as common]))

(def file-path-regex #"^(((\.\.){1}/)*|(/){1})?(([\w-]*)/)*([\w-]+)$")

(s/def ::file-path (s/and string? #(re-matches file-path-regex %)))

(s/def ::type
  (s/or :key #{:final}
        :scalar int?))

(s/def ::fire-spread ::type)
(s/def ::flame-length ::type)
(s/def ::fire-line-intensity ::type)
(s/def ::burn-history ::type)
(s/def ::output-burn-probability ::type)
(s/def ::output-directory ::file-path)

(s/def ::output-layers
  (common/one-or-more-keys
   [::burn-history
    ::fire-line-intensity
    ::fire-spread
    ::flame-length]))
