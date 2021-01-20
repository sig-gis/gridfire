(ns gridfire.spec.spotting
  (:require [clojure.spec.alpha :as s]))

(s/def ::ambient-gas-density float?)

(s/def ::specific-heat-gas float?)

(s/def ::ordered (fn [[lo hi]] (<= lo hi)))

(s/def ::crown-fire-spotting-percent (s/or
                                      :scalar (s/and float?
                                                     #(<= 0.0 % 1.0))
                                      :range (s/and (s/tuple float? float?)
                                                    ::ordered)))

(s/def ::lo (s/or
             :scalar int?
             :range (s/and (s/tuple int? int?)
                           ::ordered)))

(s/def ::hi (s/or
             :scalar int?
             :range (s/and (s/tuple int? int?)
                           ::ordered)))

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

(s/def ::spotting
  (s/keys :req-un [::ambient-gas-density
                   ::crown-fire-spotting-percent
                   ::num-firebrands
                   ::specific-heat-gas]))
