(ns puasson.core
  (:require [incanter.core :as i]
            [incanter.charts :as c]))

(def sparse-overload-coef (Math/pow 1.618 2))

(defn transpose
  [matrix]
  (apply mapv vector matrix))

(defn fp
  [lambda k]
  (min 1 (/ (Math/exp (- (/ (Math/pow (- k lambda) 2)
                            (* 2 lambda))))
            (Math/sqrt (* 2 Math/PI lambda)))))

(def cf (comp int #(Math/ceil %)))

(defn gxf
  [a b x]
  (Math/exp (+ (* a (Math/log x)) b)))

(def nf (partial gxf -0.24 8.42))
(def stepf (partial gxf 0.41 -2.51))

(defn fps
  [lambda]
  (let [N (int (nf lambda))
        step (stepf lambda)
        ks (map (partial * step)
                (range 0 N))]
    ;; (println "l=" lambda " N=" N " step=" step)
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

(defn limit-top
  [total max0 rpx]
  (let [max-rpx (/ total sparse-overload-coef)
        sparse-coef (/ rpx max-rpx)
        sparse? (> sparse-coef 1)]
    [sparse?
     (if sparse?
       [(/ max0 sparse-coef) (cf (/ rpx sparse-coef))]
       [max0 (cf rpx)])]))

(defn calc-lambda-rps
  [percentil total lambda1]
  (let [rf (comp second (partial prange percentil))
        max01 (rf lambda1)
        rph01 (* max01 3600)
        [sparse1? [max1 rph]] (limit-top total max01 rph01)
        lambda2 max1
        max02 (rf lambda2)
        rpm02 (cf (* max02 60))
        [sparse2? [max2 rpm]] (limit-top rph max02 rpm02)
        lambda3 max2
        max03 (rf lambda3)
        rps03 (cf max03)
        [sparse3? [_ rps]] (limit-top rpm max03 rps03)]
    [(or sparse1? sparse2? sparse3?)
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
   (triples 80 day-go-n day-region-n grow-percent))
  ([percentil day-go-n day-region-n grow-percent]
   (let [gp (+ 1 (/ grow-percent 100))
         total (* (+ day-go-n day-region-n) gp)
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
      :total total
      :average {:ph (float aph)
                :pm (float apm)
                :ps (float aps)}
      :sparse? sparse?
      :peak {:ph rph
             :pm rpm
             :ps rps}
      :P (or p 0)})))

(comment

  (triples (+ 8000 110000)
           (+ 12000 190000)
           50)

  (triples 6000 9000 0)
  (triples 20000 0)

  (triples 3200 50)
  (triples 100 1180 2020 50)

  (triples 1400 100)
  (triples 98 500 900 100)

  (triples 30000000)
  (triples 99.98 5000000 25000000 0)


  (nf 0.07)
  (nf 669.6429)
  (nf 800)
  (nf 10000)

  ;; (prange 98 0.07097222)
  ;; (prange 99.95 0.07097222)
  ;; (prange 99.95 0.283860491112)
  ;; (prange 99.95 1.1353284202515552)

  ;; (- 1 (/ (int (* 99.95 10000)) 1000000))

  ;; (->> (fps 0.07097222)
  ;;      (drop-while (comp (partial > 5) first))
  ;;      (take 5))

  ;; (take-last 10 (fps 0.00097222))

  ;; (->> (fps 0.07097222)
  ;;      (map (fn [[k p]]
  ;;             [k (- p 1/2000)]))
  ;;      (filter (comp neg? second))
  ;;      )

  )
