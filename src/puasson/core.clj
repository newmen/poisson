(ns puasson.core
  (:require [incanter.core :as i]
            [incanter.charts :as c]))

(def sparse-overload-coef 5)

(defn transpose
  [matrix]
  (apply mapv vector matrix))

(defn fp
  [lambda k]
  (/ (Math/exp (- (/ (Math/pow (- k lambda) 2)
                     (* 2 lambda))))
     (Math/sqrt (* 2 Math/PI lambda))))

(defn fps
  [lambda]
  (let [N 25000
        mmx (* 500 lambda)
        step (/ mmx N)
        rg (range 0 N)
        ks (map (partial * step) rg)]
    (transpose [ks
                (map (partial fp lambda) ks)])))

(defn prange
  ([lambda]
   (prange 98 lambda))
  ([percentil lambda]
   (let [min-p (- 1 (/ (int (* percentil 10000)) 1000000))
         kps (->> (fps lambda)
                  (filter (comp (partial < min-p) second)))]
     (if (empty? kps)
       [lambda lambda]
       [(first (first kps)) (first (last kps))]))))

(defn chart
  ([lambda title]
   (chart 99.95 lambda title))
  ([persentil lambda title]
   (let [[mn mx] (prange persentil lambda)
         kps (->> (fps lambda)
                  (drop-while (comp (partial > mn) first))
                  (take-while (comp (partial > mx) first)))
         x (map first kps)
         y (map second kps)]
     (i/view
      (c/xy-plot x y
                 :legend true
                 :x-label "N"
                 :y-label "P"
                 :series-label "Puasson distribution"
                 :title title)))))

(defn calc-lambda-rps
  [percentil total aps]
  (let [rf (comp second (partial prange percentil))
        cf (comp int #(Math/ceil %))
        lambda1 aps
        max0 (rf lambda1)
        rpx (* max0 3600)
        sparse? (> rpx total)
        [max1 rph] (if sparse?
                     [(/ max0 sparse-overload-coef) (cf (/ rpx sparse-overload-coef))]
                     [max0 (cf rpx)])
        lambda2 max1
        max2 (rf lambda2)
        rpm (cf (* max2 60))
        lambda3 max2
        max3 (rf lambda3)
        rps (cf max3)]
    [sparse?
     [[lambda1 rph]
      [lambda2 rpm]
      [lambda3 rps]]]))

(defn triples
  ([day-n]
   (triples day-n 0))
  ([day-n grow-percent]
   (let [day-go-n (int (* 0.37 day-n))
         day-region-n (- day-n day-go-n)]
     (triples day-go-n day-region-n grow-percent)))
  ([day-go-n day-region-n grow-percent]
   (triples 98 day-go-n day-region-n grow-percent))
  ([percentil day-go-n day-region-n grow-percent]
   (let [total (+ day-go-n day-region-n)
         gp (+ 1 (/ grow-percent 100))
         dgn (* day-go-n gp)
         drn (* day-region-n gp)
         aph (+ (/ dgn 8) (/ drn 14))
         apm (/ aph 60)
         aps (/ apm 60)
         [sparse?
          [[lambda1 rph]
           [lambda2 rpm]
           [lambda3 rps]]] (calc-lambda-rps percentil total aps)
         p (->> (fps aps)
                (drop-while (comp (partial > rps) first))
                first
                second)]
     (chart lambda1 (str "Peak per Hour: " rph))
     (chart lambda2 (str "Peak per Minute: " rpm))
     (chart lambda3 (str "Peak per Second: " rps))
     {:percentil percentil
      :aph (float aph)
      :apm (float apm)
      :aps (float aps)
      :sparse? sparse?
      :rph rph
      :rpm rpm
      :rps rps
      :P p})))

(comment

  (triples (+ 8000 40000)
           (+ 12000 60000)
           50)

  (triples 6000 9000 0)

  (triples 3200 50)
  (triples 100 1180 2020 50)

  (triples 1400 100)

  ;; (prange 98 0.07097222)
  ;; (prange 99.95 0.07097222)
  ;; (prange 99.95 0.283860491112)
  ;; (prange 99.95 1.1353284202515552)

  ;; (- 1 (/ (int (* 99.95 10000)) 1000000))

  (->> (fps 0.07097222)
       (drop-while (comp (partial > 5) first))
       (take 5))

  ;; (take-last 10 (fps 0.00097222))

  ;; (->> (fps 0.07097222)
  ;;      (map (fn [[k p]]
  ;;             [k (- p 1/2000)]))
  ;;      (filter (comp neg? second))
  ;;      )

  )
