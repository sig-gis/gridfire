(ns gridfire.conversion)

(defn F->K
  "convert farenheight to kelvin"
  [degrees]
  (->> (+ degrees 459.67)
       (* (/ 5.0 9.0))))

(defn F->C
  "convert farenheight to celcius"
  [degrees]
  (->> (- degrees 32.0)
       (* (/ 5.0 9.0))))

(defn deg->rad
  "Convert degrees to radians"
  [d]
  (* d (/ Math/PI 180)))

(defn rad->deg
  "Convert radians to degrees"
  [d]
  (* d (/ 180 Math/PI)))

(defn m->ft
  "Convert meters to feet"
  [m]
  (* m 3.281))
