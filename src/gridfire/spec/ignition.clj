(ns gridfire.spec.ignition
  (:require [clojure.spec.alpha :as s]))

;;-----------------------------------------------------------------------------
;; Regex
;;-----------------------------------------------------------------------------

(def postgis-sql-regex #"[a-z0-9]+(\.[a-z0-9]+)? WHERE rid=[0-9]+")
(def path-to-geotiff-regex #"(\/[a-z_\-\s0-9\.]+)*\.tif")

(s/def ::sql (s/and string? #(re-matches postgis-sql-regex %)))
(s/def ::path (s/and string? #(re-matches path-to-geotiff-regex %)))

;;-----------------------------------------------------------------------------

(s/def ::unburned float?)
(s/def ::burned float?)
(s/def ::burning float?)

(s/def ::values
  (s/keys :req-un [::unburned ::burned ::burning]))


(s/def ::ignition-layer
  (s/keys :req-un [(or ::sql ::path) ::values]))

;;TODO Remove before pushing
(def test-config {:ignition-layer {:path        "test/gridfire/resources/ign.tif"
                                   :burn-values {:unburned 1.0
                                                 :burned   -1.0
                                                 :burning  0.0}}})
