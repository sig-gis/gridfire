(ns gridfire.spec.output-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [gridfire.spec.output :as output]))

(deftest scalar-test
  (let [config {:fire-spread 10}]
    (is (s/valid? ::output/output-layers config))))

(deftest final-keyword-test
  (let [config {:fire-spread :final}]
    (is (s/valid? ::output/output-layers config))))

(deftest flame-length-test
  (let [config {:flame-length 10}]
    (is (s/valid? ::output/output-layers config))))

(deftest fire-line-inentisyt-test
  (let [config {:fire-line-intensity 10}]
    (is (s/valid? ::output/output-layers config))))

(deftest test
  (let [config {:burn-time 10}]
    (is (s/valid? ::output/output-layers config))))

(deftest multiple-output-layer-test
  (let [config {:fire-spread          10
                :flame-lenth          10
                ::fire-line-intensity 10
                :burn-time            10}]
    (is (s/valid? ::output/output-layers config))))
