(ns gridfire.conversion)

(defn F->K
  "Convert farenheight to kelvin."
  [degrees]
  (->> (+ degrees 459.67)
       (* (/ 5.0 9.0))))

(defn F->C
  "Convert farenheight to celcius."
  [degrees]
  (->> (- degrees 32.0)
       (* (/ 5.0 9.0))))

(defn deg->rad
  "Convert degrees to radians."
  [d]
  (* d (/ Math/PI 180)))

(defn rad->deg
  "Convert radians to degrees."
  [d]
  (* d (/ 180 Math/PI)))

(defn m->ft
  "Convert meters to feet."
  [m]
  (* m 3.281))

(defn mph->mps
  "Convert miles per hour to meters per second."
  [s]
  (* s 0.447))

(defn Btu-ft-s->kW-m
  "Convert BTU per feet per second to kilowatt per meter."
  [Btu-ft-s]
  (/ Btu-ft-s 0.288894658272))

(defn percent->dec
  [p]
  (* p 0.001))

(defn dec->percent
  [d]
  (* d 100))

(defn sec->min
  "Convert seconds to minutes."
  [s]
  (/ s 60))
