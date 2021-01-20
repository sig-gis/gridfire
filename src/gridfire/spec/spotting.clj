(ns gridfire.spec.spotting
  (:require [clojure.spec.alpha :as s]))

(s/def ::ambient-gas-density float?)

(s/def ::specific-heat-gas float?)

(s/def ::num-firebrands int?)

(s/def ::ordered (fn [[lo hi]] (<= lo hi)))

(s/def ::crown-fire-spotting-percent (s/or
                                      :scalar (s/and float?
                                                     #(<= 0.0 % 1.0))
                                      :range (s/and (s/tuple float? float?)
                                                    ::ordered)))

(s/def ::spotting
  (s/keys :req-un [::ambient-gas-density
                   ::crown-fire-spotting-percent
                   ::num-firebrands
                   ::specific-heat-gas]))
