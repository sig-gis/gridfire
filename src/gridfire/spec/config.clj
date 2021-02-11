(ns gridfire.spec.config
  (:require [clojure.spec.alpha          :as s]
            [gridfire.spec.common        :as common]
            [gridfire.spec.ignition      :as ignition]
            [gridfire.spec.output        :as output]
            [gridfire.spec.spotting :as spotting]
            [gridfire.spec.perturbations :as perturbations]))

;;-----------------------------------------------------------------------------
;; Weather Layers ;;TODO move into own namespace
;;-----------------------------------------------------------------------------

(s/def ::weather
  (s/or :vector (s/coll-of int? :kind vector? :count 2)
        :list (s/coll-of int? :kind list?)
        :string string?
        :scalar (s/or :int int?
                      :float float?)
        :map ::common/postgis-or-geotiff))

(s/def ::temperature ::weather)
(s/def ::relative-humidity ::weather)
(s/def ::wind-speed-20ft ::weather)
(s/def ::wind-from-direction ::weather)

(s/def ::weather-layers
  (s/keys
   :req-un [::temperature ::relative-humidity ::wind-speed-20ft ::wind-from-direction]))

(def weather-names
  [:temperature :relative-humidity :wind-speed-20ft :wind-from-direction])

;;-----------------------------------------------------------------------------
;; Landfire Layers ;;TODO move into own namespace
;;-----------------------------------------------------------------------------

(s/def ::path-or-map (s/or :path ::common/path
                           :map  ::common/postgis-or-geotiff))
(s/def ::aspect ::path-or-map)
(s/def ::canopy-base-height ::path-or-map)
(s/def ::canopy-cover ::path-or-map)
(s/def ::canopy-height ::path-or-map)
(s/def ::crown-bulk-density ::path-or-map)
(s/def ::elevation ::path-or-map)
(s/def ::fuel-model ::path-or-map)
(s/def ::slope ::path-or-map)
(s/def ::cell-size float?)

(s/def ::landfire-layers
  (s/keys
   :req-un [::aspect
            ::canopy-base-height
            ::canopy-cover
            ::canopy-height
            ::crown-bulk-density
            ::elevation
            ::fuel-model
            ::slope]))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------


(s/def ::config
  (s/and
   (s/keys
    :req-un [::cell-size
             ::landfire-layers]
    :opt-un [::perturbations/perturbations
             ::ignition/ignition-layer
             ::output/output-binary
             ::output/output-layers
             ::output/output-burn-probability
             ::spotting/spotting])
   ::weather-layers))
