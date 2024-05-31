(ns poisson.users
  (:require [poisson.decompose :as d]))

(def timestamps (range 24))
(def active-sessions [268 182 125 94
                      99 128 187 283
                      400 505 648 767
                      800 798 809 810
                      784 770 756 710
                      652 580 486 369])

(defn total-active-sessions
  [active-sessions]
  (reduce + active-sessions))

(defn get-indexes
  [start-time end-time]
  [(.indexOf timestamps start-time) 
   (.indexOf timestamps end-time)])

(defn peak-active-sessions
  [start-time end-time]
  (let [[start-index end-index] (get-indexes start-time end-time)]
    (reduce + (subvec active-sessions start-index (inc end-index)))))

(defn peak-percentage
  [total-sessions peak-sessions]
  (* (/ peak-sessions total-sessions) 100))

(defn num-hours
  [start-time end-time]
  (let [[start-index end-index] (get-indexes start-time end-time)]
    (inc (- end-index start-index))))

(defn analyze-peak-sessions
  [peak-start peak-end]
  (let [total-sessions (total-active-sessions active-sessions)
        peak-sessions (peak-active-sessions peak-start peak-end)
        peak-percent (peak-percentage total-sessions peak-sessions)
        hours (num-hours peak-start peak-end)]
    {:total-sessions total-sessions
     :peak-sessions peak-sessions
     :peak-percent (float peak-percent)
     :num-hours hours
     :rate (float (/ peak-percent hours))}))

(comment

  (float (/ (reduce + active-sessions) (count active-sessions)))

  (analyze-peak-sessions 12 16)
  (analyze-peak-sessions 12 19)
  (analyze-peak-sessions 10 16)
  (analyze-peak-sessions 10 19)

  (analyze-peak-sessions 14 15)
  (analyze-peak-sessions 3 4)

  (float (/ 1619 193))

  (d/plot-decomposition (conj active-sessions
                              (first active-sessions))
                        3 6)

  )
