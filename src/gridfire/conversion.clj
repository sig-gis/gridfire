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

(defn deg->rad [d]
  (* d (/ Math/PI 180)))

(defn rad->deg [d]
  (* d (/ 180 Math/PI)))

(defn m->ft [m]
  (* m 3.281))
