(ns gridfire.spec.ignition
  (:require [clojure.spec.alpha :as s]
            [gridfire.spec.common :as common]))

(s/def ::unburned float?)
(s/def ::burned float?)

(s/def ::values
  (s/keys :req-un [::unburned ::burned]))

(s/def ::ignition-layer
  (s/keys :req-un [(or ::common/sql ::common/path) ::values]))

;;TODO Remove before pushing
(def test-config {:ignition-layer {:path        "test/gridfire/resources/ign.tif"
                                   :burn-values {:unburned 1.0
                                                 :burned   -1.0}}})
