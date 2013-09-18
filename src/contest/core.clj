(ns contest.core
  (:require [clojure.core.reducers :as r]
            [clojure.string :as s]
            [iota :as iota]))

(defn street-name [location1]
    (-> location1
        (s/replace #"\s(NORTH|SOUTH|EAST|WEST|E|W|N|S)$" "")
        (s/replace #"\s(ST|STREET|AV|AVE|COURT|CRT|CT|RD)$" "")
        (s/replace #"^\s*[\"\-!#'\(%*$\./]*" "")
        (s/replace #"^\s*([\d\.\-/!$']+[ABC]*\s*)+" "")))

(defn parse [line]
  (let [parts (s/split line #",")]
    {:set-fine-amount (when-let [set-fine-amount (get parts 4)] (Integer/parseInt set-fine-amount))
     :street-name (-> (get parts 7) (s/upper-case) (street-name))}))

(defn tree-map [m]
  (doto
    (java.util.TreeMap. #(compare (get m %2) (get m %1)))
    (.putAll m)))

(defn sort-parking-tickets [file]
  (defn reduce-parking-tickets
    ([m {:keys [set-fine-amount street-name]}]
     (update-in m [street-name] (fn [v] (if (nil? v) set-fine-amount (+ v set-fine-amount))))))
  (defn combine
    ([] {})
    ([ma mb] (merge-with + ma mb)))
  (->> (rest (iota/seq file))
       (r/map parse)
       (r/filter (comp not nil? :set-fine-amount))
       (r/filter (comp not empty? :street-name))
       (r/fold combine reduce-parking-tickets)
       (tree-map)))

(time (sort-parking-tickets "./resources/Parking_Tags_Data_2012.csv"))


