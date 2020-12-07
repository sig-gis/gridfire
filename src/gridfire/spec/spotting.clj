(ns gridfire.spec.spotting
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.common :as common]))

(s/def ::ambient-gas-density float?)
(s/def ::firebrand-count int?)

(s/def ::spotting
  (one-or-more-keys [::ambient-gas-density
                     ::firebrand-count]))
