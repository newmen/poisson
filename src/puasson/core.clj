(ns puasson.core
  (:require [incanter.core :as i]
            [incanter.charts :as c]))

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
  (let [N 5000
        mmx (* 4 lambda)
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
         vps (fps lambda)
         kps (->> vps
                  (map (fn [[k p]]
                         [k (- p min-p)]))
                  (filter (comp neg? second)))
         no? (empty? kps)
         kps (if no? vps kps)
         lsf (comp (partial > lambda) first)
         left (take-while lsf kps)
         left (if (empty? left) [(first vps)] left)
         right (drop-while lsf kps)
         xf (if no? min-key max-key)
         msf (comp first (partial apply (partial xf second)))]
     [(msf left) (msf right)])))

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
   (let [gp (+ 1 (/ grow-percent 100))
         dgn (* day-go-n gp)
         drn (* day-region-n gp)
         aph (+ (/ dgn 8) (/ drn 14))
         apm (/ aph 60)
         aps (/ apm 60)
         rf (comp second (partial prange percentil))
         cf (comp int #(Math/ceil %))
         lambda1 aps
         max1 (rf lambda1)
         rph (cf (* max1 3600))
         lambda2 max1
         max2 (rf lambda2)
         rpm (cf (* max2 60))
         lambda3 max2
         max3 (rf lambda3)
         rps (cf max3)]
     (chart lambda1 (str "Peak per Hour: " rph))
     (chart lambda2 (str "Peak per Minute: " rpm))
     (chart lambda3 (str "Peak per Second: " rps))
     {:percentil percentil
      :aph (float aph)
      :apm (float apm)
      :aps (float aps)
      :rph rph
      :rpm rpm
      :rps rps})))

(comment

  (triples (+ 8000 40000)
           (+ 12000 60000)
           50)

  (triples 6000 9000 0)

  (triples 3200 50)
  (triples 100 1180 2020 50)

  (triples 1400 100)

  (prange 98 0.07097222)
  (prange 99.95 0.07097222)
  (prange 99.95 0.283860491112)
  (prange 99.95 1.1353284202515552)
  ;; (- 1 (/ (int (* 99.95 10000)) 1000000))

  (->> (fps 0.07097222)
       (filter (comp (partial < 8) first))
       (take 5))

  ;; (take-last 10 (fps 0.00097222))

  ;; (->> (fps 0.07097222)
  ;;      (map (fn [[k p]]
  ;;             [k (- p 1/2000)]))
  ;;      (filter (comp neg? second))
  ;;      )

  )
