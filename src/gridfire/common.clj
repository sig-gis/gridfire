(ns gridfire.common
  (:require
   [clojure.core.matrix :as m]
   [gridfire.perturbation :as perturbation]))

(defn matrix-value-at [[i j] global-clock matrix]
  (if (> (m/dimensionality matrix) 2)
    (let [band (int (quot global-clock 60.0))] ;Assuming each band is 1 hour
      (m/mget matrix band i j))
    (m/mget matrix i j)))

(defn sample-at
  [here global-clock matrix multiplier perturb-info]
  (let [cell       (if multiplier
                     (map #(quot % multiplier) here)
                     here)
        value-here (matrix-value-at cell global-clock matrix)]
    (if perturb-info
      (if-let [freq (:frequency perturb-info)]
        (+ value-here (perturbation/value-at perturb-info matrix cell (quot global-clock freq)))
        (+ value-here (perturbation/value-at perturb-info matrix cell)))
      value-here)))

(def sample-at
  (memoize sample-at))

(defn extract-constants

  [{:keys [landfire-layers wind-speed-20ft wind-from-direction temperature
           relative-humidity foliar-moisture ellipse-adjustment-factor
           multiplier-lookup perturbations]}
   global-clock
   [i j :as here]]
  (let [layers (merge landfire-layers
                      {:wind-speed-20ft     wind-speed-20ft
                       :wind-from-direction wind-from-direction
                       :temperature         temperature
                       :relative-humidity   relative-humidity})]
    (reduce-kv
     (fn[acc name val]
       (if (> (m/dimensionality val) 1)
         (assoc acc name (sample-at here
                                    global-clock
                                    val
                                    (name multiplier-lookup)
                                    (name perturbations)))
         (assoc acc name val)))
     {}
     layers)))

(defn calc-emc
  "Computes the Equilibrium Moisture Content (EMC) from rh (relative
   humidity in %) and temp (temperature in F)."
  [rh temp]
  (/ (cond (< rh 10)  (+ 0.03229 (* 0.281073 rh) (* -0.000578 rh temp))
           (< rh 50)  (+ 2.22749 (* 0.160107 rh) (* -0.01478 temp))
           :otherwise (+ 21.0606 (* 0.005565 rh rh) (* -0.00035 rh temp) (* -0.483199 rh)))
     30))

(defn fuel-moisture [relative-humidity temperature]
  (let [equilibrium-moisture (calc-emc relative-humidity temperature)]
    {:dead {:1hr   (+ equilibrium-moisture 0.002)
            :10hr  (+ equilibrium-moisture 0.015)
            :100hr (+ equilibrium-moisture 0.025)}
     :live {:herbaceous (* equilibrium-moisture 2.0)
            :woody      (* equilibrium-moisture 0.5)}}))

(defn in-bounds?
  "Returns true if the point lies within the bounds [0,rows) by [0,cols)."
  [rows cols [i j]]
  (and (>= i 0)
       (>= j 0)
       (< i rows)
       (< j cols)))

(defn burnable-fuel-model?
  [^double number]
  (and (pos? number)
       (or (< number 91.0)
           (> number 99.0))))

(defn burnable?
  "Returns true if cell [i j] has not yet been ignited (but could be)."
  [fire-spread-matrix fuel-model-matrix [i j]]
  (and (zero? (m/mget fire-spread-matrix i j))
       (burnable-fuel-model? (m/mget fuel-model-matrix i j))))

(defn distance-3d
  "Returns the terrain distance between two points in feet."
  [elevation-matrix cell-size [i1 j1] [i2 j2]]
  (let [di (* cell-size (- i1 i2))
        dj (* cell-size (- j1 j2))
        dz (- (m/mget elevation-matrix i1 j1)
              (m/mget elevation-matrix i2 j2))]
    (Math/sqrt (+ (* di di) (* dj dj) (* dz dz)))))
