(ns gridfire.spec.spotting
  (:require [clojure.spec.alpha :as s]))

(s/def ::ambient-gas-density float?)

(s/def ::specific-heat-gas float?)

(s/def ::ordered-range (fn [[lo hi]] (<= lo hi)))

(s/def ::crown-fire-spotting-percent (s/or
                                      :scalar (s/and float?
                                                     #(<= 0.0 % 1.0))
                                      :range (s/and (s/tuple float? float?)
                                                    ::ordered-range)))

(s/def ::lo (s/or
             :scalar int?
             :range (s/and (s/tuple int? int?)
                           ::ordered-range)))

(s/def ::hi (s/or
             :scalar int?
             :range (s/and (s/tuple int? int?)
                           ::ordered-range)))

(defn- increasing? [s]
  (if (seq s)
    (let [[f & rest] s]
      (if (and rest (> f (first rest)))
        false
        (recur rest)))
    true))

(s/def ::valid-numbrand-ranges
  (fn [{:keys [lo hi]}]
    (let [[_ l] lo
          [_ h] hi]
      (increasing? (flatten (conj [] l h))))))

(s/def ::num-firebrands (s/or
                         :scalar int?
                         :map (s/and (s/keys :req-un [::lo ::hi])
                                     ::valid-numbrand-ranges)))

(s/def ::valid-fuel-range (fn [[lo hi]] (<= 1 lo hi 204)))

(s/def ::fuel-number-range (s/and (s/tuple int? int?)
                                  ::valid-fuel-range
                                  ::ordered-range))

(s/def ::fuel-percent-pair (s/tuple ::fuel-number-range (s/or :scalar float?
                                                              :range (s/tuple float? float?))))

(s/def ::spotting-percent
  (s/coll-of ::fuel-percent-pair :kind vector?))

(s/def ::critical-fire-line-intensity float?)

(s/def ::surface-fire-spotting
  (s/keys :req-un [::spotting-percent ::critical-fire-line-intensity]))

(s/def ::spotting
  (s/keys :req-un [::ambient-gas-density
                   ::crown-fire-spotting-percent
                   ::num-firebrands
                   ::specific-heat-gas]
          :opt-un [::surface-fire-spotting]))
