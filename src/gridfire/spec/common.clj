(ns gridfire.spec.common
  (:require [clojure.spec.alpha :as s]))

;;-----------------------------------------------------------------------------
;; Regex
;;-----------------------------------------------------------------------------

(def postgis-sql-regex #"[a-z0-9]+(\.[a-z0-9]+)? WHERE rid=[0-9]+")
(def path-to-geotiff-regex #"(\/[a-z_\-\s0-9\.]+)*\.tif")

;;-----------------------------------------------------------------------------
;; Spec
;;-----------------------------------------------------------------------------

(s/def ::sql (s/and string? #(re-matches postgis-sql-regex %)))
(s/def ::path (s/and string? #(re-matches path-to-geotiff-regex %)))
