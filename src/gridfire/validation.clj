(ns gridfire.validation
  (:require [clojure.string :as s]))

(def weather-names
  [:temperature :relative-humidity :wind-speed-20ft :wind-from-direction])

(defn multiple?
  [target cell-size]
  (zero? (mod target cell-size)))

(defn weather-cell-size
  [{:keys [cell-size] :as config}]
  (let [weather    (map config weather-names)
        cell-sizes (->> weather
                        (filter map?)
                        (map :cell-size))]
    (if (every? #(multiple? cell-size %) cell-sizes)
      [config nil]
      [nil (str "cell-size of all weather rasters and simulation cell-size must be multiples of one another")])))

(defn weather-fetch-methods
  [config]
  (let [f                      (fn [k] (keyword (s/join "-" ["fetch" (name k) "method"])))
        weather-rasters        (->> weather-names
                                    (select-keys config)
                                    (filter (fn [[k v]] (map? v))))
        fetch-keywords         (map f (keys weather-rasters))
        missing-fetch-keywords (->> fetch-keywords
                                    (filter #(not (contains? config %)))
                                    seq)]
    (if missing-fetch-keywords
      [nil (str "Missing these fetch-<weather>-method keywords: " missing-fetch-keywords)]
      [config nil])))

(defn bind-error [f [val err]]
  (if (nil? err)
    (f val)
    [nil err]))

(defmacro err->> [val & fns]
  (let [fns (for [f fns] `(bind-error ~f))]
    `(->> [~val nil]
          ~@fns)))

(defn valid-config? [config]
  (err->> config
          weather-cell-size
          weather-fetch-methods))
