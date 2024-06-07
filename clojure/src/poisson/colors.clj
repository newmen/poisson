(ns poisson.colors
  (:require [clojure.string :as s]))

(defn dec-to-hex
  [n]
  (format "%02x" n))

(defn get-random-color
  []
  (str "#"
       (dec-to-hex (rand-int 256))
       (dec-to-hex (rand-int 256))
       (dec-to-hex (rand-int 256))))

(defn generate-russian-letter-utf8
  []
  (let [lowest-russian-letter-code-point 0x0410 ; Unicode code point for 'А'
        highest-russian-letter-code-point 0x044F ; Unicode code point for 'я'
        random-code-point (+ lowest-russian-letter-code-point
                             (rand-int (- highest-russian-letter-code-point
                                          lowest-russian-letter-code-point)))]
    (str (char random-code-point))))

(defn get-random-russian-letters
  [length-from length-to]
  (apply str
         (repeatedly (+ length-from
                        (rand-int (- length-to length-from)))
                     generate-russian-letter-utf8)))

(defn get-random-row
  []
  (str (get-random-russian-letters 5 24)
       ";"
       (get-random-color)))

(defn generate-random-csv
  [rows-n]
  (s/join "\n"
          (cons "name;hex_color"
                (repeatedly rows-n get-random-row))))

(comment
  
  (dec-to-hex 255)
  (get-random-color)

  (spit "tmp/colors.csv" (generate-random-csv 35000))
  
  )