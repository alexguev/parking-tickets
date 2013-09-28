(ns contest.core
  (:require [clojure.core.reducers :as r]
            [clojure.string :as s]
            [iota :as iota]))

(defn extract-street-name [location1]
    (-> location1
        (s/replace #"\s(NORTH|SOUTH|EAST|WEST|E|W|N|S)$" "")
        ;;(s/replace #"\s(ST|STREET|AV|AVE|COURT|CRT|CT|RD)$" "")
        (s/replace #"^\s*[\"\-!#'\(%*$\./]*" "")
        (s/replace #"^\s*([\d\.\-/!$']+[ABC]*\s*)+" "")))

(defn parse [line]
  (let [parts (s/split line #",")]
    [(let [street-name (get parts 7)]
       (extract-street-name (s/upper-case street-name)))
     (when-let [set-fine-amount (get parts 4)]
       (Long/parseLong set-fine-amount))]))

(defn tree-map [m]
  (doto
    (java.util.TreeMap. #(compare (get m %2) (get m %1)))
    (.putAll m)))

(defn sort-parking-tickets [file]

  (defn reducef
    ([m [street-name set-fine-amount]]
     (update-in m [street-name] (fn [v] (if (nil? v) set-fine-amount (+ v set-fine-amount))))))

  (defn combinef
    ([] {})
    ([ma mb] (merge-with + ma mb)))

  (->> (rest (iota/seq file))
       (r/map parse)
       (r/fold combinef reducef)
       (tree-map)))

(defn -main [& args]
  (time (sort-parking-tickets "./resources/Parking_Tags_Data_2012.csv")))

(-main)
