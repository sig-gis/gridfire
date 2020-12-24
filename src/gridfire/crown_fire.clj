;; [[file:../../org/GridFire.org::van-wagner-crown-fire-initiation][van-wagner-crown-fire-initiation]]
(ns gridfire.crown-fire)

(defn ft->m [ft] (* 0.3048 ft))

(defn kW-m->Btu-ft-s [kW-m] (* 0.288894658272 kW-m))

(defn Btu-ft-s->kW-m [Btu-ft-s] (/ Btu-ft-s 0.288894658272))

(defn van-wagner-crown-fire-initiation?
  "- canopy-cover (0-100 %)
   - canopy-base-height (ft)
   - foliar-moisture (lb moisture/lb ovendry weight)
   - fire-line-intensity (Btu/ft*s)"
  [canopy-cover canopy-base-height foliar-moisture fire-line-intensity]
  (and (> canopy-cover 40.0)
       (-> (+ 460.0 (* 2600.0 foliar-moisture)) ;; heat-of-ignition = kJ/kg
           (* 0.01 (ft->m canopy-base-height))
           (Math/pow 1.5) ;; critical-intensity = kW/m
           (kW-m->Btu-ft-s)
           (< fire-line-intensity))))
;; van-wagner-crown-fire-initiation ends here

;; [[file:../../org/GridFire.org::cruz-crown-fire-spread][cruz-crown-fire-spread]]
(defn mph->km-hr [mph] (* 1.609344 mph))

(defn lb-ft3->kg-m3 [lb-ft3] (* 16.01846 lb-ft3))

(defn m->ft [m] (* 3.281 m))

(defn cruz-crown-fire-spread
  "Returns spread-rate in ft/min given:
   - wind-speed-20ft (mph)
   - crown-bulk-density (lb/ft^3)
   - estimated-fine-fuel-moisture (-> M_f :dead :1hr) (0-1)"
  [wind-speed-20ft crown-bulk-density estimated-fine-fuel-moisture]
  (let [wind-speed-10m               (/ (mph->km-hr wind-speed-20ft) 0.87) ;; km/hr
        crown-bulk-density           (lb-ft3->kg-m3 crown-bulk-density) ;; kg/m^3
        estimated-fine-fuel-moisture (* 100.0 estimated-fine-fuel-moisture)
        active-spread-rate           (* 11.02
                                        (Math/pow wind-speed-10m 0.90)
                                        (Math/pow crown-bulk-density 0.19)
                                        (Math/exp (* -0.17 estimated-fine-fuel-moisture)))
                                        ;; m/min
        critical-spread-rate         (/ 3.0 crown-bulk-density) ;; m/min
        criteria-for-active-crowning (/ active-spread-rate critical-spread-rate)]
    (m->ft
     (if (> active-spread-rate critical-spread-rate)
       active-spread-rate
       (* active-spread-rate (Math/exp (- criteria-for-active-crowning)))))))
;; cruz-crown-fire-spread ends here

;; [[file:../../org/GridFire.org::crown-fire-line-intensity][crown-fire-line-intensity]]
;; heat of combustion is h from the fuel models (generally 8000 Btu/lb)
(defn crown-fire-line-intensity
  "(ft/min * lb/ft^3 * ft * Btu/lb)/60 = (Btu/ft*min)/60 = Btu/ft*s"
  [crown-spread-rate crown-bulk-density canopy-height canopy-base-height heat-of-combustion]
  (/ (* crown-spread-rate
        crown-bulk-density
        (- canopy-height canopy-base-height)
        heat-of-combustion)
     60.0))

(defn crown-fire-line-intensity-elmfire ;; kW/m
  [surface-fire-line-intensity crown-spread-rate crown-bulk-density
   canopy-height canopy-base-height]
  (let [heat-of-combustion 18000] ;; kJ/m^2
    (+ surface-fire-line-intensity ;; kW/m
       (/ (* 0.3048 ;; m/ft
             crown-spread-rate ;; ft/min
             crown-bulk-density ;; kg/m^3
             (- canopy-height canopy-base-height) ;; m
             heat-of-combustion) ;; kJ/kg
          60.0)))) ;; s/min
;; crown-fire-line-intensity ends here

;; [[file:../../org/GridFire.org::crown-eccentricity][crown-eccentricity]]
(defn crown-fire-eccentricity
  "mph"
  [wind-speed-20ft ellipse-adjustment-factor]
  (let [length-width-ratio (+ 1.0 (* 0.125
                                     wind-speed-20ft
                                     ellipse-adjustment-factor))]
    (/ (Math/sqrt (- (Math/pow length-width-ratio 2.0) 1.0))
       length-width-ratio)))

(defn elmfire-length-to-width-ratio
  "true/false mph int>0 ft/min
   Crown L/W = min(1.0 + 0.125*U20_mph, L/W_max)
   Surface L/W = 0.936*e^(0.2566*Ueff_mph) + 0.461*e^(-0.1548*Ueff_mph) - 0.397"
  [crown-fire? wind-speed-20ft max-length-to-width-ratio effective-wind-speed]
  (if crown-fire?
    (min (+ 1.0 (* 0.125 wind-speed-20ft)) max-length-to-width-ratio)
    (min (+ (* 0.936 (Math/exp (/ (* 0.2566 effective-wind-speed 60.0) 5280.0)))
            (* 0.461 (Math/exp (/ (* -0.1548 effective-wind-speed 60.0) 5280.0)))
            -0.397)
         8.0)))
;; crown-eccentricity ends here
