;; [[file:../../org/GridFire.org::*Spotting Model Forumulas][Spotting Model Forumulas:1]]
(ns gridfire.spotting)

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn F->K
  "convert farenheight to kelvin"
  [degrees]
  (->> (+ degrees 459.67)
       (* (/ 5 9))))

;;-----------------------------------------------------------------------------
;; Formulas
;;-----------------------------------------------------------------------------

(defn froude-number
  [wind-speed-20ft
   fire-line-intensity
   temperature
   ambient-gas-density
   specific-heat-gas]
  (let [;;gravity
        g 9.81

        ;; characteristic length of plume
        L_c (-> (/ fire-line-intensity
                   (* ambient-gas-density
                      specific-heat-gas
                      (F->K temperature)
                      (Math/sqrt g)))
                (Math/pow (/ 2 3)))]
    (/ wind-speed-20ft
       (Math/sqrt (* g L_c)))))

(defn buoyancy-driven? [froude]
  (<= froude 1))

(defn deviation-fb
  [froude fire-line-intensity wind-speed-20ft]
  (if (buoyancy-driven? froude)
    (+ (* 0.86 (Math/pow fire-line-intensity -0.21) (Math/pow wind-speed-20ft 0.44)) 0.19)
    (- (* 4.95 (Math/pow fire-line-intensity -0.01) (Math/pow wind-speed-20ft -0.02)) 3.48)))

(defn mean-fb
  [froude fire-line-intensity wind-speed-20ft]
  (if (buoyancy-driven? froude)
    (+ (* 1.47 (Math/pow fire-line-intensity 0.54) (Math/pow wind-speed-20ft -0.55)) 1.14)
    (- (* 1.32 (Math/pow fire-line-intensity 0.26) (Math/pow wind-speed-20ft 0.11)) 0.02)))

(defn sardoy-probability
  [distance mean deviation]
  (* (/ 1
        (* (Math/sqrt (* 2 Math/PI)) deviation distance))
     (Math/exp
      (/ (* -1 (Math/pow (- (Math/log distance) mean) 2))
         (* 2 (Math/pow deviation 2))))))
;; Spotting Model Forumulas:1 ends here
