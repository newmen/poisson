(ns poisson.core
  (:require [incanter.core :as i]
            [incanter.charts :as c]
            [incanter.stats :as stats]))

(defonce lambda-limit 1000)

(defn transpose
  [matrix]
  (apply mapv vector matrix))

(defn fp
  [lambda k]
  (min 1 (/ (Math/exp (- (/ (Math/pow (- k lambda) 2)
                            (* 2 lambda))))
            (Math/sqrt (* 2 Math/PI lambda)))))

(defn normal-fp
  [lambda k]
  (stats/pdf-normal k
                    :mean lambda
                    :sd (Math/sqrt lambda)))

(def cf (comp int #(Math/ceil %)))

(defn gxf
  [a b x]
  (Math/exp (+ (* a (Math/log x)) b)))

(def nf (comp int (partial gxf -0.24 8.42)))
(def stepf (partial gxf 0.41 -2.51))

(defn gxf-chart
  [f name]
  (let [x (range 1 10000)
        y (map (comp f float) x)]
    (i/view (c/xy-plot x y
                       :x-label "lambda"
                       :y-label name))))

(defn gxf-charts
  []
  (gxf-chart nf "N")
  (gxf-chart stepf "step"))

(defn get-fks
  [lambda]
  (if (> lambda lambda-limit)
    [normal-fp
     (range 0
            (int (+ lambda
                    (* 6 (Math/sqrt lambda))))
            (max 1 (Math/floor (Math/sqrt (/ lambda lambda-limit)))))]
    [fp
     (map (partial * (stepf lambda))
          (range 0 (nf lambda)))]))

(defn fps
  [lambda]
  (let [[f ks] (get-fks lambda)]
    (transpose [ks
                (map (partial f lambda) ks)])))

(defn sparse-rounded-fps
  [lambda]
  (->> (reduce (fn [acc [k p]]
                 (let [[pd pck _] (peek acc)
                       cck (cf k)
                       cd (- cck k)]
                   (if pd
                     (if (< pck cck)
                       (let [cd' (- k pck)]
                         (if (< cd' pd)
                           (conj (pop acc) [cd' pck p] [cd cck p])
                           (conj acc [cd cck p])))
                       (conj (pop acc) [cd cck p]))
                     (conj acc [cd cck p]))))
               []
               (fps lambda))
       (map (juxt second last))
       (filter (comp pos? first))))

(defn find-threshold
  [lambda target-prob]
  (let [rb (* (- 1 target-prob) 0.1)]
    (loop [cumulative-prob 0.0
           kps (sparse-rounded-fps lambda)]
      (let [[k p] (first kps)]
        (if (or (empty? (rest kps))
                (>= cumulative-prob target-prob)
                (and (> cumulative-prob (* p 10)) (< p rb))
                (and (pos? cumulative-prob) (zero? p)))
          k
          (recur (+ cumulative-prob p) (rest kps)))))))

(defn normal-quantile
  [lambda target-prob]
  (let [z (if (> target-prob 0.999999999)
            6
            (stats/quantile-normal target-prob))]
    (int (Math/ceil (+ lambda (* z (Math/sqrt lambda)))))))

(defn peak-load-estimation
  [lambda target-prob]
  (if (> lambda lambda-limit)
    (normal-quantile lambda target-prob)
    (find-threshold lambda target-prob)))

(defn limit-prob
  [prob kps]
  (let [min-p (- 1 prob)]
    (filter (comp (partial < min-p) second)
            kps)))

(defn add-filled-area
  [plot highlight-x highlight-y]
  (-> plot
      (c/add-lines highlight-x highlight-y
                   :series-label "Target probability"
                   :line-color java.awt.Color/BLUE)
      (c/set-stroke :width 3 :dataset 1)))

(defn chart
  ([lambda title]
   (chart lambda title {}))
  ([lambda title opts]
   (let [x-coef (:x-coef opts 1)
         prob (:prob opts 0.99999999)
         threshold (when-let [th (:threshold opts)]
                     (* x-coef th))
         kps (->> (fps lambda)
                  (limit-prob prob))
         x (map (comp (partial * x-coef) first) kps)
         y (map second kps)
         title' (if threshold
                  (str "Peak per " title ": " threshold)
                  title)
         base-plot (c/xy-plot x y
                              :legend true
                              :x-label "N"
                              :y-label "P"
                              :series-label "Poisson distribution"
                              :title title')]
     (if threshold
       (let [area-x (take-while (partial >= threshold) x)
             area-y (take (count area-x) y)]
         (doto base-plot
           (add-filled-area area-x area-y)
           (i/view)))
       (doto base-plot
         (i/view))))))

(defn get-total-aph
  [day-n]
  (/ (* day-n 0.1383) 2)) ; 13.83% of all requests come in interval between 13:30 and 15:30 (2 hours))

(defn triples
  ([day-n]
   (triples day-n {}))
  ([n opts]
   (let [grow (get opts :grow 0)
         prob (get opts :prob 0.95)
         aph (cond
               (not (map? n)) (get-total-aph n)
               (:aph n) (:aph n)
               (:work-day-n n) (let [wdn (:work-day-n n)]
                                 (+ (/ (* wdn 0.4) 8)
                                    (/ (* wdn 0.6) 14)))
               (:total-day-n n) (get-total-aph (:total-day-n n)))
         total (* aph 24 (inc grow))
         thh (peak-load-estimation aph prob)
         apm (/ thh 60)
         thm (peak-load-estimation apm prob)
         aps (/ thm 60)
         ths (peak-load-estimation aps prob)]
     (when (:chart opts)
       (let [xf #(hash-map :threshold %)]
         (chart aph "Hour" (xf thh))
         (chart apm "Minute" (xf thm))
         (chart aps "Second" (xf ths))))
     {:prob prob
      :total total
      :average {:ph (/ total 24.0)
                :pm (/ total (* 24 60.0))
                :ps (/ total (* 24 60 60.0))}
      :peak {:ph thh
             :pm thm
             :ps ths}})))

(comment

  (triples {:work-day-n (* 80 5 60 8)} {:prob 0.99 :chart true})

  (triples (* 6300 35/10) {:prob 0.9995 :chart true})
  (triples (* 825000 35/10) {:prob 0.9995 :chart true})

  (triples 3200 {:grow 0.5})
  (triples 3200 {:grow 0.5 :prob 0.995})

  (chart 5000 "x" {:prob 0.99995})

  (find-threshold 0.38583332 0.995)
  (normal-quantile 0.38583332 0.995)
  (find-threshold 1000 1)
  (normal-quantile 1000 1)
  (find-threshold 2474 0.99995)
  (normal-quantile 2474 0.99995)

  (gxf-charts)

  (* 12 13.84)

  (->> (fps 120312.5)
       count)
  ;; (fp 5000 69)

  ;; (nf 0.07)
  ;; (nf 669.6429)
  ;; (nf 800)
  ;; (nf 10000)

  )
