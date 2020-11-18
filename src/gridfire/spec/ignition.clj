(ns gridfire.spec.ignition
  (:require [clojure.spec.alpha :as s]
            [gridfire.spec.common :as common]))

(s/def ::burned float?)
(s/def ::unburned float?)

(s/def ::burn-values
  (s/keys :req-un [::unburned ::burned]))

(s/def ::ignition-layer
  (s/or
   :path ::common/path
   :sql ::common/sql
   :map (s/keys :req-un [(or ::common/sql ::common/path) ::burn-values])))
