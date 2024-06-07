(ns poisson.decompose
  (:require [incanter.core :as i]
            [incanter.charts :as c]))

(defn moving-average
  [data window-size]
  (let [n (count data)]
    (map #(/ (apply + (subvec data % (+ % window-size)))
             window-size)
         (range 0 (- n window-size)))))

(defn extend-moving-average
  [data window-size]
  (let [ma (moving-average data window-size)
        prefix (repeat (/ window-size 2) (first ma))
        suffix (repeat (/ window-size 2) (last ma))]
    (concat prefix ma suffix)))

(defn decompose-timeseries
  [data window-size season-length]
  (let [trend (extend-moving-average data window-size)
        detrended (map - data trend)
        seasonal (map #(apply + %) (partition season-length season-length detrended))
        seasonal-average (map #(/ % season-length) seasonal)
        seasonal-component (cycle seasonal-average)
        residual (map - detrended seasonal-component)]
    {:trend trend
     :seasonal (take (count trend) seasonal-component)
     :residual residual}))

(defn plot-decomposition
  [time-data value-data window-size season-length]
  (let [{:keys [trend seasonal residual]} (decompose-timeseries value-data window-size season-length)]
    (doto (c/xy-plot time-data value-data
                     :x-label "Time"
                     :y-label "Value"
                     :title "Decomposition of Time Series"
                     :series-label "Original Data"
                     :legend true)
      (c/add-lines time-data trend :series-label "Trend")
      (c/add-lines time-data seasonal :series-label "Seasonal")
      (c/add-lines time-data residual :series-label "Residual")
      (i/view))))
