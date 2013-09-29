(ns contest.core
  (:require [clojure.core.reducers :as r]
            [clojure.string :as s]
            [iota :as iota]))

(def suffixes #{"E" "W" "N" "S" "NORTH" "SOUTH" "EAST" "WEST"
                "ST" "CT" "RD" "AV" "AVE" "CRT" "STREET" "COURT" "CRES"
                "CRS" "ROAD" "AVENUE" "BLVD" "GARDEN" "GARDENS" "GRDNS" "CRESCENT"})

(defn extract-street-name [^String location1]
  (let [^StringBuilder sb (StringBuilder.)]
    (doseq [^String part (s/split location1 #"\s+")]
      (when-not (or (re-find #"\d" part)
                    (<= (.length part) 2)
                    (and (> (.length sb) 0) (contains? suffixes part)))
        (doto sb (.append part) (.append " "))))
    (.substring sb 0 (if (> (.length sb) 0)
                       (dec (.length sb))
                       (.length sb)))))

(defn parse [line]
  (let [[_ _ _ _ set-fine-amount _ _ street-name] (s/split line #",")]
    [(extract-street-name (s/upper-case street-name))
     (when set-fine-amount (Integer/parseInt set-fine-amount))]))

(defn tree-map [m]
  (doto
    (java.util.TreeMap. #(compare (get m %2) (get m %1)))
    (.putAll m)))

(defn sort-parking-tickets [file]

  (defn add-to-set-fine-amount [^java.util.Map m street-name set-fine-amount]
    (if-let [v (.get m street-name)]
      (doto m (.put street-name (+ v set-fine-amount)))
      (doto m (.put street-name set-fine-amount))))

  (defn merge-maps [ma mb]
    (reduce (fn [m [k v]] (add-to-set-fine-amount m k v)) mb ma))

  (defn reduce-parking-tickets
    ([m [street-name set-fine-amount]]
     (add-to-set-fine-amount m street-name set-fine-amount)))

  (defn combine
    ([] (java.util.HashMap.))
    ([ma mb] (merge-maps ma mb)))

  (->> (rest (iota/seq file))
       (r/map parse)
       (r/fold combine reduce-parking-tickets)
       (tree-map)))

(defn -main [& args]
  (time (sort-parking-tickets "./resources/Parking_Tags_Data_2012.csv")))

(-main)
