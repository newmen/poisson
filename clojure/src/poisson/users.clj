(ns poisson.users
  (:require [poisson.decompose :as d]))

(defn get-intermediates
  [coll]
  (let [xs (cycle coll)]
    (->> (map vector xs (rest xs))
         (take (count coll))
         (map (partial apply +))
         (map #(/ % 2)))))

(defn fill-intermediates
  [coll]
  (interleave coll (get-intermediates coll)))

(def wh-timestamps (range 24))
(def hh-timestamps (map #(/ % 2) (range 48)))

(defn only-hours?
  [coll]
  (= (count coll)
     (count wh-timestamps)))

(defn get-timestamps
  [active-sessions]
  (if (only-hours? active-sessions)
    wh-timestamps
    hh-timestamps))

(def active-sessions-1
  [268 182 125 94
   99 128 187 283
   400 505 648 767
   800 798 809 810
   784 770 756 710
   652 580 486 369])

(def active-half-sessions-1
  (fill-intermediates active-sessions-1))

(def active-half-sessions-2
  (->> [[2.39 2.24] [1.91 1.76] [1.53 1.42] [1.2 1.13] ; 0
        [1 0.956] [0.924 0.826] [0.911 0.802] [0.913 0.867] ; 2
        [1.01 0.936] [1.2 1.11] [1.4 1.29] [1.78 1.64] ; 4
        [2.15 1.97] [2.74 2.53] [3.3 3.08] [4.02 3.67] ; 6
        [4.57 4.13] [5.47 5.21] [5.91 5.56] [6.82 6.45] ; 8
        [8.33 6.3] [8.12 7.41] [8.3 7.87] [8.83 7.82] ; 10
        [9.06 8] [9.03 8.59] [9.35 8.79] [9.03 8.58] ; 12
        [8.89 8.68] [8.97 8.53] [9.08 8.42] [8.81 8.09] ; 14
        [8.64 8.1] [8.63 8.42] [8.58 8.11] [8.56 8.24] ; 16
        [8.63 7.79] [8.37 7.75] [7.99 7.41] [7.67 7.09] ; 18
        [7.31 6.86] [6.9 6.44] [6.42 6.02] [5.86 5.48] ; 20
        [5.21 4.64] [4.44 4.03] [3.71 3.43] [2.9 2.8]] ; 22
       (map (partial apply +))
       (map (partial * 1000))
       (map int)
       (into [])))

(def active-sessions-2
  (->> (partition 2 2 active-half-sessions-2)
       (map first)
       (into [])))

(defn total-active-sessions
  [active-sessions]
  (reduce + active-sessions))

(defn get-indexes
  [timestamps start-time end-time]
  [(.indexOf timestamps start-time) 
   (.indexOf timestamps end-time)])

(defn peak-active-sessions
  [timestamps active-sessions start-time end-time]
  (let [[start-index end-index] (get-indexes timestamps start-time end-time)]
    (reduce + (subvec active-sessions start-index (inc end-index)))))

(defn peak-percentage
  [total-sessions peak-sessions]
  (* (/ peak-sessions total-sessions) 100))

(defn count-hours
  [timestamps start-time end-time]
  (let [[start-index end-index] (get-indexes timestamps start-time end-time)]
    (inc (- end-index start-index))))

(defn calc-hours
  [timestamps start-time end-time]
  (let [hours (count-hours timestamps start-time end-time)]
    (if (only-hours? timestamps)
      hours
      (let [x (dec hours)]
        (inc (/ x 2))))))

(defn analyze-peak-sessions
  [active-sessions peak-start peak-end]
  (let [timestamps (get-timestamps active-sessions)
        total-sessions (total-active-sessions active-sessions)
        peak-sessions (peak-active-sessions timestamps active-sessions peak-start peak-end)
        peak-percent (peak-percentage total-sessions peak-sessions)
        total-hours (count-hours timestamps peak-start peak-end)
        out-hours (calc-hours timestamps peak-start peak-end)
        coef (if (only-hours? active-sessions) 1 2)
        rate (float (* coef (/ peak-percent total-hours)))]
    {:total-sessions total-sessions
     :peak-sessions peak-sessions
     :peak-percent (* out-hours rate)
     :num-hours out-hours
     :rate rate}))

(defn plot
  [active-sessions]
  (let [timestamps (get-timestamps active-sessions)]
    (d/plot-decomposition timestamps
                        (conj active-sessions
                              (first active-sessions))
                        3 6)))

(comment

  (float (/ (reduce + active-sessions-1) (count active-sessions-1)))
  (float (/ (reduce + active-sessions-2) (count active-sessions-2)))

  (analyze-peak-sessions active-sessions-2 12 16)
  (analyze-peak-sessions active-sessions-2 12 19)
  (analyze-peak-sessions active-sessions-2 10 16)
  (analyze-peak-sessions active-sessions-2 10 19)

  (analyze-peak-sessions active-sessions-2 13 14)
  (analyze-peak-sessions active-sessions-2 14 15)
  (analyze-peak-sessions active-sessions-2 3 4)

  (float (/ 49.630955 8))

  (* 3.4576585 2)

  (plot active-sessions-1)
  (plot active-half-sessions-1)
  (plot active-sessions-2)
  (plot active-half-sessions-2)

  )
